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

# Healthy: the node imports blocks after the restart (finality is watched by the alerts, since a node far behind
# finalizes only once it has caught up).
healthy() {
  local since=$1 deadline=$(( $(date +%s) + 240 ))
  while (( $(date +%s) < deadline )); do
    sleep 10
    awk -v s="$since" '$1 >= s' "$LOG" 2>/dev/null | grep -q 'imported block' && return 0
    systemctl is-failed -q jam-node && return 1
  done
  return 1
}

restart_on() {
  set_image "$1"
  local since
  since=$(date -u +%Y-%m-%dT%H:%M:%S)
  systemctl enable -q jam-node
  systemctl restart jam-node
  healthy "$since"
}

if restart_on "$IMAGE"; then
  echo "deployed $IMAGE"
  docker image prune -af --filter "until=168h" >/dev/null || true
else
  echo "no block import or finality progress on $IMAGE; rolling back to $PREVIOUS" >&2
  docker logs --tail 100 jam-node >&2 || true
  if [[ -n "$PREVIOUS" && "$PREVIOUS" != "$IMAGE" ]]; then restart_on "$PREVIOUS" || true; fi
  exit 1
fi
