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

UNIT=/etc/systemd/system/jam-node.service
OLD_ARGS='--spec /data/spec.json --data /data/db --port 40000 --seed-file /data/node.seed $JAM_ARGS'
NEW_ARGS='-c /data --chain /data/spec.json run -d /data --port 40000 $JAM_ARGS'
DATA=/var/lib/jam

takes_run() { docker run --rm "$1" run --help >/dev/null 2>&1; }

replace_in_unit() { local unit; unit=$(<"$UNIT"); printf '%s\n' "${unit/"$1"/"$2"}" > "$UNIT"; systemctl daemon-reload; }

to_new_command_line() {
  local image=$1 id peer_id args
  id=$(jq -r .id "$DATA/spec.json")
  [[ -e "$DATA/$id" || ! -e "$DATA/db" ]] || mv "$DATA/db" "$DATA/$id"
  if [[ -s "$DATA/node.seed" && ! -e "$DATA/$id/keys/node.seed" ]]; then
    mkdir -p "$DATA/$id/keys"
    python3 -c 'import sys; open(sys.argv[2], "wb").write(bytes.fromhex(open(sys.argv[1]).read().strip().removeprefix("0x")))' \
      "$DATA/node.seed" "$DATA/$id/keys/node.seed"
    chmod 600 "$DATA/$id/keys/node.seed"
  fi
  mkdir -p "$DATA/$id"
  chown -R "$(docker image inspect -f '{{.Config.User}}' "$image")" "$DATA/$id"
  peer_id=$( (docker run --rm -v "$DATA:/data" "$image" -c /data --chain /data/spec.json list-keys || true) |
    sed -n 's|.*/node.seed: Peer ID: ||p')
  cp "$ENV_FILE" "$ENV_FILE.before-new-cli"
  args=$(sed -n 's/^JAM_ARGS=//p' "$ENV_FILE")
  if [[ " $args " == *" --rpc-port "* ]]; then
    [[ " $args " == *" --rpc-listen-ip "* ]] || args+=" --rpc-listen-ip 127.0.0.1"
  else
    args+=" --no-rpc"
  fi
  sed -i "s|^JAM_ARGS=.*|JAM_ARGS=--mode ordinary${peer_id:+ --peer-id $peer_id} $args|" "$ENV_FILE"
  replace_in_unit "$OLD_ARGS" "$NEW_ARGS"
}

to_old_command_line() {
  local id image
  id=$(jq -r .id "$DATA/spec.json")
  [[ -e "$DATA/db" || ! -e "$DATA/$id" ]] || mv "$DATA/$id" "$DATA/db"
  image=$(sed -n 's/^JAM_IMAGE=//p' "$ENV_FILE")
  [[ -f "$ENV_FILE.before-new-cli" ]] && cp "$ENV_FILE.before-new-cli" "$ENV_FILE" && set_image "$image"
  replace_in_unit "$NEW_ARGS" "$OLD_ARGS"
}

command_line_for() {
  if takes_run "$1"; then
    if grep -qF -- "$OLD_ARGS" "$UNIT"; then to_new_command_line "$1"; fi
  elif grep -qF -- "$NEW_ARGS" "$UNIT"; then
    to_old_command_line
  fi
}

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
  command_line_for "$1"
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
