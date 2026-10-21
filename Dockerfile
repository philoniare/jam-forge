# Stage 1: Build native Rust libraries.
FROM rust:1.97.0-bookworm AS rust-builder

WORKDIR /build

COPY modules/crypto/native/bandersnatch-vrfs-wrapper ./bandersnatch-vrfs-wrapper
COPY modules/crypto/native/ed25519-zebra-wrapper ./ed25519-zebra-wrapper
COPY modules/crypto/native/erasure-coding-wrapper ./erasure-coding-wrapper

WORKDIR /build/bandersnatch-vrfs-wrapper
RUN cargo build --release --locked

WORKDIR /build/ed25519-zebra-wrapper
RUN cargo build --release --locked

WORKDIR /build/erasure-coding-wrapper
RUN cargo build --release --locked

FROM sbtscala/scala-sbt:eclipse-temurin-24.0.1_9_1.12.11_3.3.7 AS scala-builder

WORKDIR /build

RUN mkdir -p /build/modules/crypto/native/build/linux
COPY --from=rust-builder /build/bandersnatch-vrfs-wrapper/target/release/libbandersnatch_vrfs_wrapper.so /build/modules/crypto/native/build/linux/
COPY --from=rust-builder /build/ed25519-zebra-wrapper/target/release/libed25519_zebra_wrapper.so /build/modules/crypto/native/build/linux/
COPY --from=rust-builder /build/erasure-coding-wrapper/target/release/liberasure_coding_wrapper.so /build/modules/crypto/native/build/linux/

COPY build.sbt .
COPY project/build.properties project/
COPY project/plugins.sbt project/
COPY project/*.scala project/

RUN sbt update

COPY modules/core ./modules/core
COPY modules/crypto ./modules/crypto
COPY modules/pvm ./modules/pvm
COPY modules/protocol ./modules/protocol
COPY modules/conformance ./modules/conformance

RUN mkdir -p modules/pvm/native/build/linux && touch modules/pvm/native/build/linux/libpvm_recompiler.so

ARG GIT_SHA=unknown
ARG BUILD_TIME=unknown
ENV JAM_BUILD_GIT_SHA=${GIT_SHA}
ENV JAM_BUILD_TIME=${BUILD_TIME}

RUN echo "Building for git=${GIT_SHA} at ${BUILD_TIME}" && \
    sbt clean "conformance/assembly"

# Stage 3: Runtime image
FROM azul/zulu-openjdk:24.0.2-24.32-jdk-crac

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY --from=rust-builder /build/bandersnatch-vrfs-wrapper/target/release/libbandersnatch_vrfs_wrapper.so /app/lib/
COPY --from=rust-builder /build/ed25519-zebra-wrapper/target/release/libed25519_zebra_wrapper.so /app/lib/
COPY --from=rust-builder /build/erasure-coding-wrapper/target/release/liberasure_coding_wrapper.so /app/lib/

COPY --from=scala-builder /build/modules/conformance/target/scala-3.3.7/jam-conformance.jar /app/
COPY --from=scala-builder /build/modules/conformance/src/main/resources/logback-prod.xml /app/logback.xml
COPY jamtestvectors/traces/preimages/*.bin /app/warmup-traces/preimages/
COPY jamtestvectors/traces/preimages_light/*.bin /app/warmup-traces/preimages_light/
ENV JAM_TRACES_DIR=/app/warmup-traces

ENV LD_LIBRARY_PATH=/app/lib
ENV LOG_LEVEL=ERROR
ENV JAVA_OPTS="-XX:+UseZGC -Xms4g -Xmx4g -XX:+AlwaysPreTouch -XX:+UseStringDeduplication -XX:+ExitOnOutOfMemoryError -XX:+HeapDumpOnOutOfMemoryError -XX:HeapDumpPath=/tmp/jam_oom.hprof --enable-native-access=ALL-UNNAMED --sun-misc-unsafe-memory-access=allow -Dlogback.configurationFile=/app/logback.xml"

ARG GIT_SHA=unknown
ARG BUILD_TIME=unknown
ENV JAM_BUILD_GIT_SHA=${GIT_SHA}
ENV JAM_BUILD_TIME=${BUILD_TIME}
LABEL org.opencontainers.image.revision=${GIT_SHA}
LABEL org.opencontainers.image.created=${BUILD_TIME}

ENTRYPOINT ["/bin/sh", "-c", "exec java $JAVA_OPTS -jar /app/jam-conformance.jar \"$@\"", "--"]
CMD ["--socket-path", "/tmp/jam_target.sock"]
