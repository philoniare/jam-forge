#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

TARGET="${1:?usage: $0 <target> [trace ...]}"; shift
[[ $# -gt 0 ]] || set -- fallback safrole storage storage_light
TRACES=("$@")
CONF_DIR="${JAM_CONFORMANCE_DIR:-$PWD/jam-conformance}"
OUT="target/fuzz-perf/$TARGET"
mkdir -p "$OUT"

CP_FILE=target/fuzz-perf/classpath.txt
if [[ ! -s "$CP_FILE" || build.sbt -nt "$CP_FILE" ]]; then
  echo "== resolving classpath (sbt) =="
  sbt --error "conformance/Test/compile" >/dev/null
  sbt --error "export conformance/Test/fullClasspath" | tail -1 | sed 's#^[^/]*/#/#' > "$CP_FILE"
fi
CP="$(cat "$CP_FILE")"
# Same JVM flags as the jam-forge Docker image.
JOPTS=(-XX:+UseZGC -Xms2g -Xmx4g -XX:+AlwaysPreTouch --enable-native-access=ALL-UNNAMED
  --sun-misc-unsafe-memory-access=allow
  -Djava.library.path="$PWD/modules/crypto/native/build/linux:$PWD/modules/crypto/src/main/resources"
  -Dlogback.configurationFile="$PWD/modules/conformance/src/main/resources/logback-prod.xml")

DATA="$(mktemp -d /tmp/jam_fuzz_perf.XXXXXX)"
TARGET_PID=""
stop_target() {
  if [[ -n "$TARGET_PID" ]]; then kill "$TARGET_PID" 2>/dev/null || true; wait "$TARGET_PID" 2>/dev/null || true; fi
  TARGET_PID=""
}
trap 'stop_target; rm -rf "$DATA"' EXIT

if [[ "$TARGET" != jam-forge ]]; then
  python3 "$CONF_DIR/scripts/target.py" get "$TARGET"
fi

for trace in "${TRACES[@]}"; do
  echo "== $TARGET / $trace =="
  rm -rf "$DATA" && mkdir -p "$DATA" && chmod 777 "$DATA"
  if [[ "$TARGET" == jam-forge ]]; then
    java "${JOPTS[@]}" -cp "$CP" io.forge.jam.conformance.ConformanceServerApp fuzz "$DATA/fuzz.sock" \
      > "$OUT/$trace.target.log" 2>&1 &
  else
    JAM_FUZZ_DATA_PATH="$DATA" python3 "$CONF_DIR/scripts/target.py" run "$TARGET" > "$OUT/$trace.target.log" 2>&1 &
  fi
  TARGET_PID=$!
  java -cp "$CP" io.forge.jam.conformance.FuzzPerfReplay --socket "$DATA/fuzz.sock" \
    --trace "jamtestvectors/traces/$trace" --out "$OUT/$trace.json" 2>&1 | grep -E '^RESULT|unexpected' || true
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
