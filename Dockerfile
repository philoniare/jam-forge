# Stage 1: Build native Rust libraries (using nightly for edition2024 support)
FROM rust:latest AS rust-builder

RUN rustup install nightly && rustup default nightly

WORKDIR /build

COPY modules/crypto/native/ark-vrf ./ark-vrf
COPY modules/crypto/native/bandersnatch-vrfs-wrapper ./bandersnatch-vrfs-wrapper
COPY modules/crypto/native/ed25519-zebra-wrapper ./ed25519-zebra-wrapper
COPY modules/crypto/native/erasure-coding-wrapper ./erasure-coding-wrapper

WORKDIR /build/bandersnatch-vrfs-wrapper
RUN cargo build --release

WORKDIR /build/ed25519-zebra-wrapper
RUN cargo build --release

WORKDIR /build/erasure-coding-wrapper
RUN cargo build --release

# Stage 2: Build Scala application
FROM sbtscala/scala-sbt:eclipse-temurin-21.0.5_11_1.10.6_3.3.4 AS scala-builder

WORKDIR /build

RUN mkdir -p /build/modules/crypto/native/build/linux
COPY --from=rust-builder /build/bandersnatch-vrfs-wrapper/target/release/libbandersnatch_vrfs_wrapper.so /build/modules/crypto/native/build/linux/
COPY --from=rust-builder /build/ed25519-zebra-wrapper/target/release/libed25519_zebra_wrapper.so /build/modules/crypto/native/build/linux/
COPY --from=rust-builder /build/erasure-coding-wrapper/target/release/liberasure_coding_wrapper.so /build/modules/crypto/native/build/linux/

COPY build.sbt .
COPY project/build.properties project/
COPY project/plugins.sbt project/

RUN sbt update

COPY modules/core ./modules/core
COPY modules/crypto ./modules/crypto
COPY modules/pvm ./modules/pvm
COPY modules/protocol ./modules/protocol
COPY modules/conformance ./modules/conformance

ARG GIT_SHA=unknown
ARG BUILD_TIME=unknown
ENV JAM_BUILD_GIT_SHA=${GIT_SHA}
ENV JAM_BUILD_TIME=${BUILD_TIME}

RUN echo "Building for git=${GIT_SHA} at ${BUILD_TIME}" && \
    sbt clean "conformance/assembly"

# Stage 3: "Cold" runtime image — CRaC-enabled JDK
FROM azul/zulu-openjdk:21-jdk-crac-latest

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY --from=rust-builder /build/bandersnatch-vrfs-wrapper/target/release/libbandersnatch_vrfs_wrapper.so /app/lib/
COPY --from=rust-builder /build/ed25519-zebra-wrapper/target/release/libed25519_zebra_wrapper.so /app/lib/
COPY --from=rust-builder /build/erasure-coding-wrapper/target/release/liberasure_coding_wrapper.so /app/lib/

COPY --from=scala-builder /build/modules/conformance/target/scala-3.3.7/jam-conformance.jar /app/
COPY --from=scala-builder /build/modules/conformance/src/main/resources/logback-prod.xml /app/logback.xml

# Traces are needed during the warmup/checkpoint creation step.
COPY jam-conformance/fuzz-reports/0.7.2/traces /app/jam-conformance/fuzz-reports/0.7.2/traces

ENV LD_LIBRARY_PATH=/app/lib
ENV JAM_TRACES_DIR=/app/jam-conformance/fuzz-reports/0.7.2/traces
ENV LOG_LEVEL=ERROR
ENV JAVA_OPTS="-XX:+UseG1GC -Xms1g -Xmx2g -XX:+AlwaysPreTouch -XX:+UseStringDeduplication -XX:+ExitOnOutOfMemoryError -XX:+HeapDumpOnOutOfMemoryError -XX:HeapDumpPath=/tmp/jam_oom.hprof -XX:CRaCAllowedOpenFilePrefixes=/proc/,/sys/,/dev/ -Dlogback.configurationFile=/app/logback.xml"

ARG GIT_SHA=unknown
ARG BUILD_TIME=unknown
ENV JAM_BUILD_GIT_SHA=${GIT_SHA}
ENV JAM_BUILD_TIME=${BUILD_TIME}
LABEL org.opencontainers.image.revision=${GIT_SHA}
LABEL org.opencontainers.image.created=${BUILD_TIME}

# Default invocation creates a CRaC checkpoint at /app/cr and then exits.
ENTRYPOINT ["/bin/sh", "-c", "exec java $JAVA_OPTS -XX:CRaCCheckpointTo=/app/cr -jar /app/jam-conformance.jar warmup-and-serve \"$@\"", "--"]
CMD ["--socket-path", "/tmp/jam_target.sock"]
