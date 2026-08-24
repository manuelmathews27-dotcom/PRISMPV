#!/usr/bin/env bash
# refresh_cohort.sh — Re-pull the FAERS reference cohort and reload PRISM.
# Intended to run quarterly via cron (FAERS updates quarterly with ~6 month lag).
#
# Usage:
#   ./scripts/refresh_cohort.sh            # run interactively
#   ./scripts/refresh_cohort.sh --quiet    # cron mode (log file only, no stdout)
#   ./scripts/refresh_cohort.sh --check    # preflight ONLY — verifies assumptions, changes nothing
#
# ─────────────────────────────────────────────────────────────────────────────
# REWRITTEN 2026-08-24. The previous version was broken and dangerous:
#   * ran the pipeline in bare `rocker/shiny:latest`, which has no dplyr —
#     it died 23s in on 2026-07-01 ("there is no package called 'dplyr'"),
#     which is why data/ sat at 2026-04-09. Now uses prism-local:latest,
#     whose Dockerfile installs the full package set.
#   * called the 3 stage scripts directly, bypassing run_pipeline.R's PRR
#     regression-test gate and its inter-stage assertions. Now calls the driver.
#   * rebuilt `prism:latest` and re-ran the container on `-p 8088:3838` with no
#     bind-mount. The live container is `prism-local:latest` on `3838:3838` WITH
#     `repo/` bind-mounted. Had the pull ever succeeded, this would have taken
#     the site down (Caddy points at :3838) and destroyed hot-reload.
#
# The rebuild/recreate path is now gone entirely. Because `repo/` is bind-mounted
# into the live container, pipeline output under repo/data/ is visible to it
# immediately — a plain `docker restart` is enough to reload it, and there is no
# port or image name left to get wrong.
# ─────────────────────────────────────────────────────────────────────────────
#
# NOTE — this refresh ships publicly. repo/data/*.rds is tracked in git, and
# `data/**` is not in deploy.yml's paths-ignore, so a successful refresh will be
# auto-committed to edward-auto and auto-deployed to the public shinyapps.io CV
# link. That is intended (fresh data should ship) but it is not reversible by
# doing nothing. To prevent a half-written .rds from being committed mid-run,
# this script holds the auto-sync cron's own flock for its whole duration, so
# the watcher no-ops until the refresh is done and the data lands as one commit.

set -Eeuo pipefail   # -E so the ERR trap is inherited by functions

PRISM_DIR="/home/manny/prism"
REPO_DIR="${PRISM_DIR}/repo"
LOG_FILE="${PRISM_DIR}/logs/refresh.log"
CONTAINER_NAME="prism"
IMAGE="prism-local:latest"
MOUNT_TARGET="/srv/shiny-server/prism"
SYNC_LOCK="/tmp/prism-auto-sync.lock"   # MUST match /home/manny/prism/scripts/auto-sync.sh
HEALTH_URL="http://localhost:3838/"
BACKUP_DIR=""

# mkdir BEFORE any redirect into the log file — `exec >> "$LOG_FILE"` fails if the
# directory does not exist yet.
mkdir -p "$(dirname "$LOG_FILE")"

MODE="run"
case "${1:-}" in
    --quiet) exec >> "$LOG_FILE" 2>&1 ;;   # stdout -> log; log() must NOT tee as well
    --check) MODE="check" ;;
    "")      ;;
    *)       echo "unknown argument: $1" >&2; exit 2 ;;
esac

# Write to the log file, and additionally to the terminal only when stdout is a
# TTY. (The old version used `tee -a` unconditionally, so in --quiet mode — where
# stdout is already redirected into the log — every line was written twice.)
log() {
    local line="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo "$line" >> "$LOG_FILE"
    [[ -t 1 ]] && echo "$line" || true
}

die() { log "ERROR: $1"; exit 1; }

# ── Preflight ────────────────────────────────────────────────────────────────
# Every assumption this script depends on, checked before anything is touched.
preflight() {
    log "Preflight checks..."

    command -v docker >/dev/null 2>&1 \
        || die "docker not available to $(whoami). Run this as edward — manny is deliberately not in the docker group."
    docker info >/dev/null 2>&1 \
        || die "cannot reach the docker daemon as $(whoami)."

    [[ -d "$REPO_DIR" ]]                 || die "repo dir missing: $REPO_DIR"
    [[ -f "$REPO_DIR/run_pipeline.R" ]]  || die "run_pipeline.R missing in $REPO_DIR"

    docker image inspect "$IMAGE" >/dev/null 2>&1 \
        || die "image $IMAGE not found. Build it: cd $PRISM_DIR && docker build -t $IMAGE -f Dockerfile ."

    docker ps --format '{{.Names}}' | grep -qx "$CONTAINER_NAME" \
        || die "container '$CONTAINER_NAME' is not running. Start it before refreshing."

    # The restart-only reload is correct ONLY if repo/ is really bind-mounted.
    # If the container was recreated without the mount, data would be baked in
    # and a restart would silently serve stale numbers — so fail loudly instead.
    local mounted
    mounted="$(docker inspect -f \
        '{{range .Mounts}}{{if eq .Destination "'"$MOUNT_TARGET"'"}}{{.Source}}{{end}}{{end}}' \
        "$CONTAINER_NAME" 2>/dev/null || true)"
    [[ -n "$mounted" ]] \
        || die "container '$CONTAINER_NAME' has no bind-mount at $MOUNT_TARGET. A restart would NOT pick up new data. Recreate it with: -v $REPO_DIR:$MOUNT_TARGET"
    [[ "$mounted" == "$REPO_DIR" ]] \
        || die "bind-mount at $MOUNT_TARGET points at '$mounted', expected '$REPO_DIR'."

    log "  ok: docker reachable as $(whoami)"
    log "  ok: image $IMAGE present"
    log "  ok: container $CONTAINER_NAME running, repo/ bind-mounted at $MOUNT_TARGET"
}

# ── Health check ─────────────────────────────────────────────────────────────
# Mirrors deploy.yml's smoke test: 200 AND the PRISM title. A fresh container
# needs ~45-60s to load R packages before it answers, so poll rather than sleep.
wait_healthy() {
    local tries="${1:-24}" i body
    for (( i = 1; i <= tries; i++ )); do
        body="$(curl -fsS --max-time 10 "$HEALTH_URL" 2>/dev/null || true)"
        if [[ -n "$body" ]] && grep -q '<title>PRISM</title>' <<< "$body"; then
            log "  healthy on attempt ${i} (200 + PRISM title)"
            return 0
        fi
        sleep 5
    done
    return 1
}

# ── Rollback ─────────────────────────────────────────────────────────────────
# The pipeline overwrites data/*.rds in place. A partial or bad pull would
# otherwise be auto-committed and auto-deployed to the public CV link, so keep a
# snapshot and put it back on any failure.
restore_backup() {
    [[ -n "$BACKUP_DIR" && -d "$BACKUP_DIR" ]] || return 0
    log "Restoring previous data/ from ${BACKUP_DIR}..."
    rm -rf "${REPO_DIR}/data"
    cp -a "$BACKUP_DIR" "${REPO_DIR}/data"
    docker restart "$CONTAINER_NAME" >/dev/null 2>&1 || true
    if wait_healthy 24; then
        log "Rollback complete — previous cohort data is live again."
    else
        log "WARNING: rolled back data but the app is not answering. Investigate: docker logs $CONTAINER_NAME"
    fi
}

on_error() {
    local rc=$?
    log "Refresh FAILED (exit ${rc})."
    restore_backup
    log "=== PRISM cohort refresh aborted ==="
    exit "$rc"
}

# ── Main ─────────────────────────────────────────────────────────────────────
log "=== PRISM cohort refresh starting (mode: ${MODE}) ==="

preflight

if [[ "$MODE" == "check" ]]; then
    log "Preflight passed. --check requested, so nothing was changed."
    log "=== PRISM cohort refresh finished (check only) ==="
    exit 0
fi

# Hold the auto-sync lock for the whole run so the every-minute watcher no-ops
# and cannot commit a half-written .rds. auto-sync.sh uses `flock -n`, so it
# exits 0 immediately while we hold this; it resumes on the next tick after.
exec 9>"$SYNC_LOCK"
if ! flock -n 9; then
    die "auto-sync is mid-run (holds $SYNC_LOCK). Try again in a minute."
fi
log "Holding auto-sync lock — the watcher will no-op until this finishes."

trap on_error ERR

BACKUP_DIR="${PRISM_DIR}/logs/data-backup-$(date '+%Y%m%dT%H%M%S')"
cp -a "${REPO_DIR}/data" "$BACKUP_DIR"
log "Backed up current data/ to ${BACKUP_DIR}"

# One container, one driver. run_pipeline.R gates on tests/test_prr_formula.R,
# then runs the 3 stages with existence assertions between them.
# Invoked via source() rather than `Rscript run_pipeline.R` because the driver
# locates the repo root through `sys.frame(1)$ofile`, which is only populated
# under source(); -w already puts us in the right directory either way.
log "Running pipeline (test gate + FAERS pull + signal detection + visuals)."
log "  FAERS pull alone takes ~45-60 min — this is expected to be slow."
docker run --rm \
    -v "${REPO_DIR}:${MOUNT_TARGET}" \
    -w "${MOUNT_TARGET}" \
    "$IMAGE" \
    Rscript -e 'source("run_pipeline.R")' 2>&1 \
    | while IFS= read -r line; do log "  $line"; done

# pipefail already propagates a non-zero Rscript exit through the log pipe above.
# This is the second guard: assert the artifacts were actually rewritten, which
# also catches a pipeline that exits 0 having silently produced nothing.
for f in faers_raw.rds combined.rds; do
    [[ -f "${REPO_DIR}/data/${f}" ]] || die "pipeline did not produce data/${f}"
    [[ "${REPO_DIR}/data/${f}" -nt "$BACKUP_DIR" ]] \
        || die "data/${f} was not rewritten — the pipeline reported success but produced no new data."
done
log "Pipeline produced fresh faers_raw.rds and combined.rds."

# No rebuild: repo/ is bind-mounted, so the container already sees the new data.
# Restart only so app.R's top-level data load re-runs in a fresh R process.
log "Restarting ${CONTAINER_NAME} to reload data (no rebuild needed)..."
docker restart "$CONTAINER_NAME" >/dev/null

if ! wait_healthy 24; then
    die "app did not return 200 + PRISM title within ~2 min after restart."
fi

trap - ERR

# Keep the 4 most recent backups (one year at quarterly cadence).
find "${PRISM_DIR}/logs" -maxdepth 1 -type d -name 'data-backup-*' \
    | sort -r | tail -n +5 | xargs -r rm -rf
log "Pruned old data backups (kept 4 most recent)."

log "Refresh complete. Fresh data is live locally."
log "  The auto-sync watcher will commit data/ to edward-auto within ~60s,"
log "  which triggers the shinyapps.io deploy — the public CV link updates itself."
log "=== PRISM cohort refresh finished ==="
