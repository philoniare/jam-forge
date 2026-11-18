#!/usr/bin/env bash
# Runs on the VM (through `az vm run-command invoke`): pull IMAGE, restart the node on it and check that it imports
# blocks. Rolls back to the previous image when it does not.
#
#   deploy.sh <image> [<ghcr user> <ghcr token>]
set -euo pipefail

IMAGE="$1"
ENV_FILE=/etc/jam-node/env
LOG=/var/log/jam-node/node.log

if [[ -n "${2:-}" && -n "${3:-}" ]]; then
  echo "$3" | docker login ghcr.io -u "$2" --password-stdin >/dev/null
fi
docker pull -q "$IMAGE"
[[ -n "${3:-}" ]] && docker logout ghcr.io >/dev/null || true

PREVIOUS=$(sed -n 's/^JAM_IMAGE=//p' "$ENV_FILE")
set_image() { sed -i "s|^JAM_IMAGE=.*|JAM_IMAGE=$1|" "$ENV_FILE"; }

restarts() { local n; n=$(systemctl show -p NRestarts --value jam-node 2>/dev/null || true); echo "${n:-0}"; }

# Healthy: the node imports blocks after the restart (finality is watched by the alerts, since a node far behind
# finalizes only once it has caught up). A node that exits is started again by Restart=always, so the unit is never
# "failed": one automatic restart since ours fails the check at once.
healthy() {
  local since=$1 restarted=$2 deadline=$(( $(date +%s) + 240 ))
  while (( $(date +%s) < deadline )); do
    sleep 10
    awk -v s="$since" '$1 >= s' "$LOG" 2>/dev/null | grep -q 'imported block' && return 0
    systemctl is-failed -q jam-node && return 1
    (( $(restarts) != restarted )) && return 1
  done
  return 1
}

restart_on() {
  set_image "$1"
  local since restarted user
  user=$(docker image inspect -f '{{.Config.User}}' "$1" 2>/dev/null || true)
  systemctl enable -q jam-node
  systemctl stop jam-node
  [[ -z "$user" ]] || chown -R "$user" /var/lib/jam /var/log/jam-node
  since=$(date -u +%Y-%m-%dT%H:%M:%S)
  systemctl start jam-node
  restarted=$(restarts)
  healthy "$since" "$restarted"
}

if restart_on "$IMAGE"; then
  echo "deployed $IMAGE"
  docker image prune -af --filter "until=168h" >/dev/null || true
else
  echo "no block import or finality progress on $IMAGE; rolling back to $PREVIOUS" >&2
  # The container is removed when it exits (--rm), so its output is read from the unit's journal.
  journalctl -u jam-node -n 40 --no-pager -o cat >&2 || true
  if [[ -n "$PREVIOUS" && "$PREVIOUS" != "$IMAGE" ]]; then restart_on "$PREVIOUS" || true; fi
  exit 1
fi
