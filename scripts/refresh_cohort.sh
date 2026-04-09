#!/usr/bin/env bash
# refresh_cohort.sh — Re-pull FAERS reference cohort data and rebuild PRISM container
# Intended to run quarterly via cron (FAERS updates quarterly with ~6 month lag)
#
# What it does:
#   1. Runs the 3-stage R pipeline (FAERS pull → signal detection → visualizations)
#   2. Rebuilds the Docker image with fresh data
#   3. Restarts the container
#
# Usage:
#   ./scripts/refresh_cohort.sh          # run interactively
#   ./scripts/refresh_cohort.sh --quiet  # cron mode (logs to file, no stdout)

set -euo pipefail

PRISM_DIR="/home/edward/apps/prism"
REPO_DIR="${PRISM_DIR}/repo"
LOG_FILE="${PRISM_DIR}/logs/refresh.log"
CONTAINER_NAME="prism"

mkdir -p "$(dirname "$LOG_FILE")"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Redirect all output to log file in quiet mode
if [[ "${1:-}" == "--quiet" ]]; then
    exec >> "$LOG_FILE" 2>&1
fi

log "=== PRISM cohort refresh starting ==="

# Run the pipeline inside a temporary R container with the repo mounted
log "Stage 1/3: Pulling FAERS data (this takes ~45-60 min)..."
docker run --rm \
    -v "${REPO_DIR}:/srv/shiny-server/prism" \
    -w /srv/shiny-server/prism \
    rocker/shiny:latest \
    Rscript scripts/01_faers_pull.R 2>&1 | tail -5 | while read -r line; do log "  $line"; done

log "Stage 2/3: Running signal detection..."
docker run --rm \
    -v "${REPO_DIR}:/srv/shiny-server/prism" \
    -w /srv/shiny-server/prism \
    rocker/shiny:latest \
    Rscript scripts/02_signal_detection.R 2>&1 | tail -5 | while read -r line; do log "  $line"; done

log "Stage 3/3: Generating visualizations..."
docker run --rm \
    -v "${REPO_DIR}:/srv/shiny-server/prism" \
    -w /srv/shiny-server/prism \
    rocker/shiny:latest \
    Rscript scripts/03_visualizations.R 2>&1 | tail -5 | while read -r line; do log "  $line"; done

# Rebuild and restart the container
log "Rebuilding Docker image..."
docker build -t prism:latest "$PRISM_DIR" 2>&1 | tail -3 | while read -r line; do log "  $line"; done

log "Restarting container..."
docker stop "$CONTAINER_NAME" 2>/dev/null || true
docker rm "$CONTAINER_NAME" 2>/dev/null || true
docker run -d \
    --name "$CONTAINER_NAME" \
    -p 8088:3838 \
    --restart unless-stopped \
    prism:latest

# Wait for startup
sleep 5
if docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    log "Container is running. Refresh complete."
else
    log "ERROR: Container failed to start. Check docker logs $CONTAINER_NAME"
    exit 1
fi

log "=== PRISM cohort refresh finished ==="
