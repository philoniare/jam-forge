#!/usr/bin/env bash
# Build jam-node, fetch the jamtoaster public testnet chain spec and run a node that syncs it.
#
#   scripts/run-jamtoaster.sh [extra jam-node args...]
#
# Env: DATA_DIR (default target/jamtoaster/data, kept across runs), PORT (default 40000),
#      SPEC_URL (default the public testnet spec), SKIP_BUILD=1 to reuse the last jar.
set -euo pipefail
cd "$(dirname "$0")/.."

SPEC_URL="${SPEC_URL:-https://jamtoaster.network/chainspec/public.json}"
WORK_DIR=target/jamtoaster
DATA_DIR="${DATA_DIR:-$WORK_DIR/data}"
PORT="${PORT:-40000}"
# Native libraries are built (by cargo, during compile) into the directory the build names after the OS.
case "$(uname -s)" in
  Darwin) OS_DIR=mac ;;
  Linux) OS_DIR=linux ;;
  *) echo "unsupported OS: $(uname -s) (macOS and Linux only)" >&2; exit 1 ;;
esac

need() { command -v "$1" >/dev/null 2>&1 || { echo "missing required tool: $1" >&2; exit 1; }; }
need java
need curl
if [[ "${SKIP_BUILD:-}" != 1 ]]; then
  need sbt
  need cargo
  echo "== building jam-node =="
  sbt -batch node/assembly
fi

JAR=$(ls -t modules/node/target/scala-*/jam-node.jar 2>/dev/null | head -1 || true)
[[ -n "$JAR" ]] || { echo "jam-node.jar not found: run without SKIP_BUILD=1" >&2; exit 1; }

mkdir -p "$WORK_DIR" "$DATA_DIR"
echo "== fetching chain spec from $SPEC_URL =="
curl -fsSL -o "$WORK_DIR/spec.json" "$SPEC_URL"

echo "== starting node (data: $DATA_DIR, port: $PORT) =="
exec java --enable-native-access=ALL-UNNAMED \
  -Djava.library.path="modules/crypto/native/build/$OS_DIR" \
  -Djam.base.dir="$PWD" \
  -jar "$JAR" \
  --spec "$WORK_DIR/spec.json" \
  --data "$DATA_DIR" \
  --port "$PORT" \
  --finality-peers 1 \
  "$@"
