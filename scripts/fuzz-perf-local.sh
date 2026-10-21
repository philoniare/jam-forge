#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

TARGET="${1:?usage: $0 <target> [trace ...]}"; shift
[[ $# -gt 0 ]] || set -- fallback safrole storage storage_light
TRACES=("$@")
CONF_DIR="${JAM_CONFORMANCE_DIR:-$PWD/jam-conformance}"
OUT="${FUZZ_PERF_OUT:-target/fuzz-perf/$TARGET}"
mkdir -p "$OUT"

CP_FILE=target/fuzz-perf/classpath.txt
mkdir -p target/fuzz-perf
if [[ ! -s "$CP_FILE" || build.sbt -nt "$CP_FILE" ]]; then
  echo "== resolving classpath (sbt) =="
  sbt --error "conformance/Test/compile" >/dev/null
  sbt -no-colors -Dsbt.supershell=false --error "export conformance/Test/fullClasspath" | grep -E '[.]jar' | tail -1 \
    | sed 's#^[^/]*/#/#' > "$CP_FILE"
fi
CP="$(cat "$CP_FILE")"
# Same JVM flags as the jam-forge Docker image (Dockerfile JAVA_OPTS).
HEAP="${JAM_HEAP:-4g}"
JOPTS=(-XX:+UseZGC -Xms"$HEAP" -Xmx"$HEAP" -XX:+AlwaysPreTouch -XX:+UseStringDeduplication -XX:+ExitOnOutOfMemoryError
  --enable-native-access=ALL-UNNAMED --sun-misc-unsafe-memory-access=allow
  -Djava.library.path="$PWD/modules/crypto/native/build/linux:$PWD/modules/crypto/src/main/resources"
  -Dlogback.configurationFile="$PWD/modules/conformance/src/main/resources/logback-prod.xml")
# Warmup corpus, laid out as the image's /app/warmup-traces.
WARMUP_DIR="$(mktemp -d /tmp/jam_warmup.XXXXXX)"
ln -s "$PWD/jamtestvectors/traces/preimages" "$WARMUP_DIR/preimages"
ln -s "$PWD/jamtestvectors/traces/preimages_light" "$WARMUP_DIR/preimages_light"

DATA="$(mktemp -d /tmp/jam_fuzz_perf.XXXXXX)"
TARGET_PID=""
stop_target() {
  if [[ -n "$TARGET_PID" ]]; then kill "$TARGET_PID" 2>/dev/null || true; wait "$TARGET_PID" 2>/dev/null || true; fi
  TARGET_PID=""
}
trap 'stop_target; rm -rf "$DATA" "$WARMUP_DIR"' EXIT

if [[ "$TARGET" != jam-forge ]]; then
  python3 "$CONF_DIR/scripts/target.py" get "$TARGET"
fi

for trace in "${TRACES[@]}"; do
  echo "== $TARGET / $trace =="
  rm -rf "$DATA" && mkdir -p "$DATA" && chmod 777 "$DATA"
  if [[ "$TARGET" == jam-forge ]]; then
    JAM_FUZZ=1 JAM_FUZZ_SPEC=tiny JAM_FUZZ_DATA_PATH="$DATA" JAM_FUZZ_SOCK_PATH="$DATA/fuzz.sock" LOG_LEVEL=ERROR \
      JAM_TRACES_DIR="$WARMUP_DIR" JAM_WARMUP="${JAM_WARMUP:-1}" \
      java "${JOPTS[@]}" -cp "$CP" io.forge.jam.conformance.ConformanceServerApp > "$OUT/$trace.target.log" 2>&1 &
  else
    JAM_FUZZ_DATA_PATH="$DATA" python3 "$CONF_DIR/scripts/target.py" run "$TARGET" > "$OUT/$trace.target.log" 2>&1 &
  fi
  TARGET_PID=$!
  java -cp "$CP" io.forge.jam.conformance.FuzzPerfReplay --socket "$DATA/fuzz.sock" \
    --trace "jamtestvectors/traces/$trace" --out "$OUT/$trace.json" --wait-secs 600 2>&1 | grep -E '^RESULT|unexpected' || true
  stop_target
done

echo "================= SUMMARY ($TARGET, ms per block) ================="
printf "%-14s %9s %9s %9s %9s\n" trace mean p50 p99 max
for trace in "${TRACES[@]}"; do
  [[ -s "$OUT/$trace.json" ]] || { printf "%-14s %s\n" "$trace" "no result (see $OUT/$trace.target.log)"; continue; }
  python3 -c 'import json,sys; s=json.load(open(sys.argv[1]))["stats"]
print("%-14s %9.2f %9.2f %9.2f %9.2f" % (sys.argv[2], s["import_mean"], s["import_p50"], s["import_p99"], s["import_max"]))' \
    "$OUT/$trace.json" "$trace"
done
