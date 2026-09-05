#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

ITERS="${1:-200000}"
SEED="${2:-}"

JHOME=""
for cand in /opt/homebrew/opt/openjdk@25 /opt/homebrew/opt/openjdk@26 /opt/homebrew/opt/openjdk; do
  if [[ -x "$cand/bin/javac" ]]; then
    ver=$("$cand/bin/javac" -version 2>&1 | grep -oE '[0-9]+' | head -1)
    if [[ "$ver" -ge 22 ]]; then JHOME="$cand"; break; fi
  fi
done
[[ -z "$JHOME" ]] && { echo "no JDK 22+ found (FFM requires it)"; exit 2; }
echo "using JDK: $("$JHOME/bin/javac" -version 2>&1)"

# --- build the Rust cdylib -------------------------------------------------
echo "== cargo build --release =="
( cd modules/pvm/native/pvm-recompiler && cargo build --release )

case "$(uname -s)" in
  Darwin) LIB=libpvm_recompiler.dylib; OSDIR=mac ;;
  Linux)  LIB=libpvm_recompiler.so;   OSDIR=linux ;;
  *) echo "unsupported OS"; exit 2 ;;
esac
DEST="modules/pvm/native/build/$OSDIR"
mkdir -p "$DEST"
cp "modules/pvm/native/pvm-recompiler/target/release/$LIB" "$DEST/$LIB"

# --- compile + run the differential harness --------------------------------
OUT=target/pvm-recompiler-difftest
mkdir -p "$OUT"
echo "== javac (FFM binding + differential harness) =="
"$JHOME/bin/javac" -d "$OUT" \
  modules/pvm/src/main/java/io/forge/jam/pvm/native_/PvmRecompiler.java \
  modules/pvm/native/ffm-binding/io/forge/jam/pvm/native_/PvmDiffTest.java

echo "== differential run: $ITERS iterations =="
"$JHOME/bin/java" --enable-native-access=ALL-UNNAMED -cp "$OUT" \
  io.forge.jam.pvm.native_.PvmDiffTest "$DEST/$LIB" "$ITERS" ${SEED:+"$SEED"}
