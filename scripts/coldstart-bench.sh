#!/usr/bin/env bash
# Usage: scripts/coldstart-bench.sh [trace]   (default: storage)
set -euo pipefail
cd "$(dirname "$0")/.."

TRACE="${1:-storage}"
MAIN=io.forge.jam.protocol.benchmark.ColdStartBenchmark
OUT=target/coldstart
mkdir -p "$OUT"

# Tuned flags: generational ZGC, fixed pre-touched heap, no tiered warmup shortcuts.
TUNED=(-XX:+UseZGC -XX:+ZGenerational -Xms2g -Xmx2g -XX:+AlwaysPreTouch)

CP_FILE="$OUT/classpath.txt"
if [[ ! -s "$CP_FILE" || build.sbt -nt "$CP_FILE" ]]; then
  echo "== resolving classpath (sbt) =="
  sbt --error "protocol/Test/compile" >/dev/null
  sbt --error "export protocol/Test/fullClasspath" | tail -1 > "$CP_FILE"
fi
CP="$(cat "$CP_FILE")"

run() { # run <log> <label> <extra-main-args...> -- <jvm-flags...>
  local log="$1" label="$2"; shift 2
  local main_args=() jvm_flags=()
  local in_flags=0
  for a in "$@"; do
    if [[ "$a" == "--" ]]; then in_flags=1; continue; fi
    if [[ $in_flags == 1 ]]; then jvm_flags+=("$a"); else main_args+=("$a"); fi
  done
  echo "== $label =="
  java ${jvm_flags[@]+"${jvm_flags[@]}"} -cp "$CP" "$MAIN" --trace "$TRACE" --label "$label" \
    ${main_args[@]+"${main_args[@]}"} | tee "$OUT/$log"
  echo
}

run a-baseline.log  "A-baseline-cold"                                  --
run b-tunedgc.log   "B-tuned-gc"                                       -- "${TUNED[@]}"
run c-warmup.log    "C-tuned+warmup"    --warmup-rounds 3              -- "${TUNED[@]}"

# D: AppCDS — dynamic archives refuse directory classpath entries, so jar the
# class directories first (the production Docker image ships jars anyway).
echo "== D: jarring class directories for CDS =="
JARS_DIR="$OUT/jars"; mkdir -p "$JARS_DIR"
CDS_CP=""
i=0
IFS=':' read -ra CP_ENTRIES <<< "$CP"
for e in "${CP_ENTRIES[@]}"; do
  if [[ -d "$e" ]]; then
    i=$((i+1))
    jarfile="$(pwd)/$JARS_DIR/dir$i.jar"
    jar cf "$jarfile" -C "$e" .
    entry="$jarfile"
  else
    entry="$e"
  fi
  CDS_CP="${CDS_CP:+$CDS_CP:}$entry"
done

echo "== D: AppCDS training run (untimed) =="
CP="$CDS_CP" run cds-training.log "cds-training" --warmup-rounds 1 -- \
  "${TUNED[@]}" -XX:ArchiveClassesAtExit="$OUT/app.jsa" >/dev/null || true
if [[ -s "$OUT/app.jsa" ]]; then
  CP="$CDS_CP" run d-cds.log "D-tuned+warmup+CDS" --warmup-rounds 3 -- \
    "${TUNED[@]}" -XX:SharedArchiveFile="$OUT/app.jsa"
else
  echo "AppCDS archive not produced — skipping D"
fi

echo "================= SUMMARY ($TRACE, one measured pass per fresh JVM) ================="
grep -h "^RESULT" "$OUT"/*.log
