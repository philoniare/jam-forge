#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

MARGIN="${MARGIN:-1.30}"
BASELINE="${PERF_BASELINE:-scripts/perf-baseline.local.txt}"
OUT=target/perf-check.log

echo "== running Phase0Benchmark =="
sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.Phase0Benchmark" > "$OUT" 2>&1 || {
  tail -5 "$OUT"; echo "benchmark failed"; exit 2; }

# Extract "trace mean" pairs from the results table
parse() {
  grep -E "^\[info\] (fallback|safrole|storage|storage_light) " "$1" \
    | awk '{print $2, $4}'
}
CURRENT="$(parse "$OUT")"
echo "$CURRENT" | awk '{printf "  %-15s mean %6.2f ms\n", $1, $2}'

if [[ "${1:-}" == "--update-baseline" ]]; then
  echo "$CURRENT" > "$BASELINE"
  echo "baseline written to $BASELINE"
  exit 0
fi

if [[ ! -s "$BASELINE" ]]; then
  echo "no baseline at $BASELINE — run with --update-baseline first"
  exit 2
fi

echo "== comparing against $BASELINE (margin ${MARGIN}x) =="
FAIL=0
while read -r trace mean; do
  base=$(awk -v t="$trace" '$1==t {print $2}' "$BASELINE")
  [[ -z "$base" ]] && continue
  limit=$(awk -v b="$base" -v m="$MARGIN" 'BEGIN{printf "%.2f", b*m}')
  ok=$(awk -v c="$mean" -v l="$limit" 'BEGIN{print (c<=l) ? "ok" : "REGRESSION"}')
  printf "  %-15s %6.2f ms (baseline %6.2f, limit %6.2f) %s\n" "$trace" "$mean" "$base" "$limit" "$ok"
  [[ "$ok" == "REGRESSION" ]] && FAIL=1
done <<< "$CURRENT"

exit $FAIL
