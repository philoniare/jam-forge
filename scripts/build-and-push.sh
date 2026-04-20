#!/bin/bash
set -euo pipefail

# Build and push jam-forge Docker image to GitHub Container Registry.
# Requires: GH_USER and GH_DOCKER_TOKEN environment variables.
#
# Flags:
#   --use-cache     Reuse docker builder cache (default: --no-cache for full rebuild).
#   --skip-verify   Skip the post-build verification step.
#   --tag <tag>     Additional tag to push alongside :latest (default: git short SHA).

IMAGE_NAME="jam-forge"
REGISTRY="ghcr.io"

NO_CACHE="--no-cache"
SKIP_VERIFY=""
EXTRA_TAG=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --no-cache) NO_CACHE="--no-cache"; shift ;;
        --use-cache) NO_CACHE=""; shift ;;
        --skip-verify) SKIP_VERIFY=1; shift ;;
        --tag) EXTRA_TAG="$2"; shift 2 ;;
        -h|--help)
            grep -E '^# ' "$0" | sed 's/^# //' | head -20
            exit 0 ;;
        *) echo "Unknown flag: $1" >&2; exit 1 ;;
    esac
done

if [ -z "${GH_USER:-}" ]; then
    echo "Error: GH_USER environment variable is not set" >&2
    exit 1
fi

if [ -z "${GH_DOCKER_TOKEN:-}" ]; then
    echo "Error: GH_DOCKER_TOKEN environment variable is not set" >&2
    exit 1
fi

# Navigate to project root (where Dockerfile is)
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

FULL_IMAGE_LATEST="${REGISTRY}/${GH_USER}/${IMAGE_NAME}:latest"
FULL_IMAGE_TAG="${REGISTRY}/${GH_USER}/${IMAGE_NAME}:${EXTRA_TAG}"

echo "=== Building ${IMAGE_NAME} for linux/amd64 ==="
echo "  git:        ${GIT_SHA}${GIT_DIRTY}"
echo "  built-at:   ${BUILD_TIME}"
echo "  tags:       :latest, :${EXTRA_TAG}"
echo "  no-cache:   ${NO_CACHE:-(use cache)}"
[ -n "$GIT_DIRTY" ] && echo "  WARNING: working tree has uncommitted changes"

echo ""
echo "=== Step 1: Removing any existing local ${IMAGE_NAME}:latest image ==="
docker image rm -f "${IMAGE_NAME}:latest" 2>/dev/null || true
docker image rm -f "${FULL_IMAGE_LATEST}" 2>/dev/null || true
docker image rm -f "${FULL_IMAGE_TAG}" 2>/dev/null || true

echo ""
echo "=== Step 2: Building Docker image ==="
docker build \
    --platform linux/amd64 \
    --pull \
    $NO_CACHE \
    --build-arg GIT_SHA="${GIT_SHA}${GIT_DIRTY}" \
    --build-arg BUILD_TIME="${BUILD_TIME}" \
    -t "${IMAGE_NAME}:latest" \
    .

IMAGE_ID="$(docker image inspect -f '{{.Id}}' "${IMAGE_NAME}:latest")"
echo "=== Built image id: ${IMAGE_ID} ==="

if [ -z "$SKIP_VERIFY" ]; then
    echo ""
    echo "=== Step 3: Verifying the built image contains current source ==="
    TMP_DIR="$(mktemp -d)"
    trap "rm -rf $TMP_DIR" EXIT
    CID="$(docker create --platform linux/amd64 "${IMAGE_NAME}:latest")"
    docker cp "$CID:/app/jam-conformance.jar" "$TMP_DIR/jam-conformance.jar"
    docker rm -f "$CID" >/dev/null

    JAR_SIZE="$(stat -f%z "$TMP_DIR/jam-conformance.jar" 2>/dev/null || stat -c%s "$TMP_DIR/jam-conformance.jar")"
    echo "  jar size: ${JAR_SIZE} bytes"

    JAR_ENTRIES="$(unzip -l "$TMP_DIR/jam-conformance.jar")"
    if ! echo "$JAR_ENTRIES" | grep -F 'io/forge/jam/protocol/report/ReportTransition' >/dev/null; then
        echo "ERROR: jam-conformance.jar has no ReportTransition entries at all — aborting push." >&2
        echo "--- jar head ---" >&2
        echo "$JAR_ENTRIES" | head -20 >&2
        exit 1
    fi
    if ! echo "$JAR_ENTRIES" | grep -F 'io/forge/jam/protocol/report/ReportTransition$.class' >/dev/null; then
        echo "ERROR: jam-conformance.jar is missing ReportTransition\$.class — aborting push." >&2
        echo "--- found ReportTransition entries ---" >&2
        echo "$JAR_ENTRIES" | grep -F 'ReportTransition' >&2 || true
        exit 1
    fi

    # Quick smoke test: the server prints its version line and responds to --help.
    if ! docker run --rm --platform linux/amd64 --entrypoint java "${IMAGE_NAME}:latest" \
            -jar /app/jam-conformance.jar --help 2>/dev/null | grep -q 'JAM Forge'; then
        echo "ERROR: smoke test failed — image doesn't respond to --help." >&2
        exit 1
    fi
    echo "  OK"
fi

echo ""
echo "=== Step 4: Logging into GHCR ==="
echo "$GH_DOCKER_TOKEN" | docker login "$REGISTRY" -u "$GH_USER" --password-stdin

echo ""
echo "=== Step 5: Tagging image ==="
docker tag "${IMAGE_NAME}:latest" "$FULL_IMAGE_LATEST"
docker tag "${IMAGE_NAME}:latest" "$FULL_IMAGE_TAG"

echo ""
echo "=== Step 6: Pushing to GHCR ==="
docker push "$FULL_IMAGE_LATEST"
docker push "$FULL_IMAGE_TAG"

echo ""
echo "=== Done! ==="
echo "  ${FULL_IMAGE_LATEST}"
echo "  ${FULL_IMAGE_TAG}"
echo "  image id: ${IMAGE_ID}"