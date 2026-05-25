#!/bin/bash
set -euo pipefail

# Build and push jam-forge Docker image to GitHub Container Registry.
# The image ships with a pre-baked CRaC checkpoint so `docker run` restores a
# warm JVM and immediately starts the conformance fuzz server.
#

IMAGE_NAME="jam-forge"
COLD_IMAGE="jam-forge-cold"
REGISTRY="ghcr.io"

NO_CACHE="--no-cache"
SKIP_VERIFY=""
SKIP_PUSH=""
EXTRA_TAG=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --no-cache) NO_CACHE="--no-cache"; shift ;;
        --use-cache) NO_CACHE=""; shift ;;
        --skip-verify) SKIP_VERIFY=1; shift ;;
        --skip-push) SKIP_PUSH=1; shift ;;
        --tag) EXTRA_TAG="$2"; shift 2 ;;
        -h|--help)
            grep -E '^# ' "$0" | sed 's/^# //' | head -25
            exit 0 ;;
        *) echo "Unknown flag: $1" >&2; exit 1 ;;
    esac
done

if [ -z "$SKIP_PUSH" ]; then
    if [ -z "${GH_USER:-}" ]; then
        echo "Error: GH_USER environment variable is not set" >&2
        exit 1
    fi
    if [ -z "${GH_DOCKER_TOKEN:-}" ]; then
        echo "Error: GH_DOCKER_TOKEN environment variable is not set" >&2
        exit 1
    fi
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

GIT_SHA="$(git rev-parse --short HEAD 2>/dev/null || echo unknown)"
GIT_DIRTY=""
if ! git diff --quiet HEAD 2>/dev/null; then
    GIT_DIRTY="-dirty"
fi
BUILD_TIME="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

: "${EXTRA_TAG:=$GIT_SHA$GIT_DIRTY}"

FULL_IMAGE_LATEST="${REGISTRY}/${GH_USER:-local}/${IMAGE_NAME}:latest"
FULL_IMAGE_TAG="${REGISTRY}/${GH_USER:-local}/${IMAGE_NAME}:${EXTRA_TAG}"
echo "=== Building ${IMAGE_NAME} (CRaC) for linux/amd64 ==="
echo "  git:        ${GIT_SHA}${GIT_DIRTY}"
echo "  built-at:   ${BUILD_TIME}"
echo "  tags:       :latest, :${EXTRA_TAG}"
echo "  no-cache:   ${NO_CACHE:-(use cache)}"
[ -n "$GIT_DIRTY" ] && echo "  WARNING: working tree has uncommitted changes"

echo ""
echo "=== Step 1: Remove existing local images ==="
docker image rm -f "${IMAGE_NAME}:latest" 2>/dev/null || true
docker image rm -f "${COLD_IMAGE}:latest" 2>/dev/null || true
docker image rm -f "${FULL_IMAGE_LATEST}" 2>/dev/null || true
docker image rm -f "${FULL_IMAGE_TAG}" 2>/dev/null || true

echo ""
echo "=== Step 2: Build cold image (${COLD_IMAGE}:latest) ==="
docker build \
    --platform linux/amd64 \
    --pull \
    $NO_CACHE \
    --build-arg GIT_SHA="${GIT_SHA}${GIT_DIRTY}" \
    --build-arg BUILD_TIME="${BUILD_TIME}" \
    -t "${COLD_IMAGE}:latest" \
    -f Dockerfile \
    .

echo ""
echo "=== Step 3: Produce CRaC checkpoint inside container ==="
CHECKPOINT_CONTAINER="jam-crac-checkpoint"
docker rm -f "$CHECKPOINT_CONTAINER" 2>/dev/null || true

set +e
docker run \
    --platform linux/amd64 \
    --privileged \
    --security-opt seccomp=unconfined \
    --security-opt apparmor=unconfined \
    --name "$CHECKPOINT_CONTAINER" \
    "${COLD_IMAGE}:latest"
set -e

# Verify a checkpoint was produced inside the container
CR_LISTING_FILE="$(mktemp)"
trap 'rm -f "$CR_LISTING_FILE"' EXIT
if ! docker cp "$CHECKPOINT_CONTAINER:/app/cr" - 2>/dev/null | tar -tvf - >"$CR_LISTING_FILE" 2>/dev/null; then
    echo "ERROR: /app/cr does not exist in checkpoint container" >&2
    echo "(Leaving $CHECKPOINT_CONTAINER for inspection — remove with: docker rm -f $CHECKPOINT_CONTAINER)" >&2
    exit 1
fi

echo "Checkpoint directory contents (/app/cr):"
cat "$CR_LISTING_FILE"

CR_FILE_COUNT="$(awk 'BEGIN{n=0} /^-/ && $NF != "cr/" {n++} END{print n}' "$CR_LISTING_FILE")"
if [ "${CR_FILE_COUNT:-0}" -lt 1 ]; then
    echo "ERROR: /app/cr is empty — no checkpoint produced." >&2
    echo "Common causes:" >&2
    echo "  - Docker Desktop on macOS: VM lacks FPU ptrace (run on Linux host)." >&2
    echo "  - Linux host with default seccomp/apparmor: ensure --security-opt unconfined." >&2
    echo "  - CRaC engine refused to checkpoint open resources (check the JVM log above for [crac] / warp lines)." >&2
    echo "(Leaving $CHECKPOINT_CONTAINER for inspection — remove with: docker rm -f $CHECKPOINT_CONTAINER)" >&2
    exit 1
fi
echo "Checkpoint produced: ${CR_FILE_COUNT} file(s) in /app/cr"

echo ""
echo "=== Step 4: Commit container as runtime image (${IMAGE_NAME}:latest) ==="
# Override CMD so `docker run` restores from the baked-in checkpoint instead of
# running the warmup-and-serve workflow again.
docker commit \
    --change 'ENTRYPOINT ["/bin/sh", "-c", "exec java $JAVA_OPTS -XX:CRaCRestoreFrom=/app/cr"]' \
    --change 'CMD []' \
    "$CHECKPOINT_CONTAINER" \
    "${IMAGE_NAME}:latest" >/dev/null
docker rm -f "$CHECKPOINT_CONTAINER" >/dev/null

IMAGE_ID="$(docker image inspect -f '{{.Id}}' "${IMAGE_NAME}:latest")"
echo "=== Built runtime image id: ${IMAGE_ID} ==="

if [ -z "$SKIP_VERIFY" ]; then
    echo ""
    echo "=== Step 5: Verify the cold image contains current source ==="
    TMP_DIR="$(mktemp -d)"
    CID="$(docker create --platform linux/amd64 "${COLD_IMAGE}:latest")"
    docker cp "$CID:/app/jam-conformance.jar" "$TMP_DIR/jam-conformance.jar"
    docker rm -f "$CID" >/dev/null

    JAR_SIZE="$(stat -f%z "$TMP_DIR/jam-conformance.jar" 2>/dev/null || stat -c%s "$TMP_DIR/jam-conformance.jar")"
    echo "  jar size: ${JAR_SIZE} bytes"

    JAR_ENTRIES="$(unzip -l "$TMP_DIR/jam-conformance.jar")"
    if ! echo "$JAR_ENTRIES" | grep -F 'io/forge/jam/protocol/report/ReportTransition' >/dev/null; then
        echo "ERROR: jam-conformance.jar has no ReportTransition entries — aborting." >&2
        exit 1
    fi
    if ! echo "$JAR_ENTRIES" | grep -F 'io/forge/jam/protocol/report/ReportTransition$.class' >/dev/null; then
        echo "ERROR: jam-conformance.jar is missing ReportTransition\$.class — aborting." >&2
        exit 1
    fi

    # Smoke test the cold image (--help works without a checkpoint context).
    if ! docker run --rm --platform linux/amd64 --entrypoint java "${COLD_IMAGE}:latest" \
            -jar /app/jam-conformance.jar --help 2>/dev/null | grep -q 'JAM Forge'; then
        echo "ERROR: smoke test failed — cold image doesn't respond to --help." >&2
        exit 1
    fi
    rm -rf "$TMP_DIR"
    echo "  OK"
fi

if [ -n "$SKIP_PUSH" ]; then
    echo ""
    echo "=== Skipping push (--skip-push). Local image: ${IMAGE_NAME}:latest ==="
    echo "    Run with: docker run --rm --privileged -v /tmp:/tmp ${IMAGE_NAME}:latest"
    exit 0
fi

echo ""
echo "=== Step 6: Log into GHCR ==="
echo "$GH_DOCKER_TOKEN" | docker login "$REGISTRY" -u "$GH_USER" --password-stdin

echo ""
echo "=== Step 7: Tag and push ==="
docker tag "${IMAGE_NAME}:latest" "$FULL_IMAGE_LATEST"
docker tag "${IMAGE_NAME}:latest" "$FULL_IMAGE_TAG"
docker push "$FULL_IMAGE_LATEST"
docker push "$FULL_IMAGE_TAG"

echo ""
echo "=== Done ==="
echo "  ${FULL_IMAGE_LATEST}"
echo "  ${FULL_IMAGE_TAG}"
echo "  image id: ${IMAGE_ID}"
