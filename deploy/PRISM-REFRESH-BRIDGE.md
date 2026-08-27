# PRISM refresh privilege bridge

These files are the version-controlled source for Edward's narrow Manny-to-Docker refresh bridge:

- `scripts/refresh_cohort.sh` → reviewed copy installed root-owned at `/usr/local/bin/prism-refresh`
- `deploy/prism-docker-bridge` → installed root-owned at `/usr/local/libexec/prism-docker-bridge`
- `deploy/manny-prism-refresh.sudoers` → installed root-owned at `/etc/sudoers.d/manny-prism-refresh`

Manny runs `prism-refresh --check` and then `prism-refresh` without a leading sudo. The coordinator performs all repo, log, backup, and rollback work as Manny. Its only privileged calls are the three exact bridge verbs named in sudoers: `check`, `run-pipeline`, and `restart`.

The pipeline verb fixes the image, bind mount, working directory, command, and container user. It runs with Manny's host UID/GID, all Linux capabilities dropped, a read-only container root filesystem, and only `/home/manny/prism/repo` writable. The Docker socket is never mounted.

Do not point cron or sudoers at the Manny-writable source files. Source changes take effect only after Edward reviews them, validates shell/sudoers syntax, and reinstalls root-owned copies. The quarterly cron switches to Manny before invoking the installed coordinator.
