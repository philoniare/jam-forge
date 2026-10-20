#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

PJ_VERSION=v0.8.0
PJ_NAME="polkajam-$PJ_VERSION-linux-x86_64"
PJ_URL="https://github.com/paritytech/polkajam-releases/releases/download/$PJ_VERSION/$PJ_NAME.tgz"
PJ_SHA256=e90c8ccd07d392c91a12da6db59bfb916f2c0158928c97468a51461f9ecfdfeb
MAIN=io.forge.jam.protocol.benchmark.PolkajamComparisonBenchmark
OUT=target/polkajam
mkdir -p "$OUT"

PJ_BIN="$OUT/$PJ_NAME/polkajam"
if [[ ! -x "$PJ_BIN" ]]; then
  echo "== downloading $PJ_NAME =="
  curl -fsSL "$PJ_URL" -o "$OUT/$PJ_NAME.tgz"
  echo "$PJ_SHA256  $OUT/$PJ_NAME.tgz" | sha256sum -c -
  tar -xzf "$OUT/$PJ_NAME.tgz" -C "$OUT"
fi
PJ_BIN="$(find "$OUT/$PJ_NAME" -type f -name polkajam -perm -u+x | head -1)"

# PolkaJam's compiler backend uses userfaultfd; without this its benchmark aborts with EPERM.
if [[ "$(cat /proc/sys/vm/unprivileged_userfaultfd 2>/dev/null || echo 1)" != 1 ]]; then
  echo 1 > /proc/sys/vm/unprivileged_userfaultfd 2>/dev/null \
    || sudo -n sh -c 'echo 1 > /proc/sys/vm/unprivileged_userfaultfd' 2>/dev/null \
    || echo "warning: vm.unprivileged_userfaultfd is 0; polkajam's compiler backend may fail" >&2
fi

echo "== polkajam benchmark =="
"$PJ_BIN" benchmark 2>&1 | grep -v "Bootstrap Service" | tee "$OUT/polkajam.log"

CP_FILE="$OUT/classpath.txt"
if [[ ! -s "$CP_FILE" || build.sbt -nt "$CP_FILE" ]]; then
  echo "== resolving classpath (sbt) =="
  sbt --error "protocol/Test/compile" >/dev/null
  sbt --error "export protocol/Test/fullClasspath" | tail -1 > "$CP_FILE"
fi

echo "== jam-forge benchmark =="
java -Xms4g -Xmx12g -XX:+UseG1GC --enable-native-access=ALL-UNNAMED \
  -Djava.library.path="$PWD/modules/crypto/native/build/linux:$PWD/modules/crypto/src/main/resources" \
  -Djam.base.dir="$PWD" -cp "$(cat "$CP_FILE")" "$MAIN" "$@" 2>/dev/null | tee "$OUT/jamforge.log"

# PolkaJam prints prose; pull out the numbers that line up with our RESULT names.
pj() { grep -oP "$1" "$OUT/polkajam.log" | head -1 || true; }
declare -A PJ=(
  [trie_populate_1m]="$(pj 'Trie populated in \K[0-9]+')"
  [trie_insert_1m]="$(pj 'insert 1M entries: \K[0-9]+')"
  [ec_bundle_encode]="$(pj 'Encoded 13Mb bundle in \K[0-9]+')"
  [ec_bundle_reconstruct]="$(pj 'Reconstructed 13Mb bundle in \K[0-9]+')"
  [ec_segments_encode]="$(pj 'segments in \K[0-9]+')"
  [ec_segments_reconstruct]="$(pj 'Reconstructed segments in \K[0-9]+')"
)

echo
echo "================= SUMMARY (same machine, ms) ================="
printf "%-26s %12s %12s %8s\n" workload polkajam jam-forge ratio
grep -h "^RESULT" "$OUT/jamforge.log" | while read -r _ name value unit; do
  ref="${PJ[$name]:-}"
  if [[ -n "$ref" && "$ref" != 0 ]]; then
    printf "%-26s %12s %12s %7.1fx\n" "$name" "$ref" "$value" "$(echo "$value / $ref" | bc -l)"
  else
    printf "%-26s %12s %12s %8s\n" "$name" "-" "$value $unit" ""
  fi
done
echo "polkajam block import (its own generated 1000-block chain; not the same workload as import_*):"
grep -h "^Imported" "$OUT/polkajam.log" || true
