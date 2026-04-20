# Stage 1: Build native Rust libraries (using nightly for edition2024 support)
FROM rust:latest AS rust-builder

# Install nightly toolchain for edition2024 support
RUN rustup install nightly && rustup default nightly

WORKDIR /build

# Copy Rust projects for native libraries (including ark-vrf dependency)
COPY modules/crypto/native/ark-vrf ./ark-vrf
COPY modules/crypto/native/bandersnatch-vrfs-wrapper ./bandersnatch-vrfs-wrapper
COPY modules/crypto/native/ed25519-zebra-wrapper ./ed25519-zebra-wrapper
COPY modules/crypto/native/erasure-coding-wrapper ./erasure-coding-wrapper

# Build Bandersnatch VRF library
WORKDIR /build/bandersnatch-vrfs-wrapper
RUN cargo build --release

# Build Ed25519-Zebra library
WORKDIR /build/ed25519-zebra-wrapper
RUN cargo build --release

# Build Erasure Coding library
WORKDIR /build/erasure-coding-wrapper
RUN cargo build --release

# Stage 2: Build Scala application
FROM sbtscala/scala-sbt:eclipse-temurin-21.0.5_11_1.10.6_3.3.4 AS scala-builder

WORKDIR /build

# Copy native libraries from rust builder
RUN mkdir -p /build/modules/crypto/native/build/linux
COPY --from=rust-builder /build/bandersnatch-vrfs-wrapper/target/release/libbandersnatch_vrfs_wrapper.so /build/modules/crypto/native/build/linux/
COPY --from=rust-builder /build/ed25519-zebra-wrapper/target/release/libed25519_zebra_wrapper.so /build/modules/crypto/native/build/linux/
COPY --from=rust-builder /build/erasure-coding-wrapper/target/release/liberasure_coding_wrapper.so /build/modules/crypto/native/build/linux/

# Copy build files first for better caching
COPY build.sbt .
COPY project/build.properties project/
COPY project/plugins.sbt project/

# Fetch dependencies (cached layer)
RUN sbt update

# Copy source code
COPY modules/core ./modules/core
COPY modules/crypto ./modules/crypto
COPY modules/pvm ./modules/pvm
COPY modules/protocol ./modules/protocol
COPY modules/conformance ./modules/conformance

ARG GIT_SHA=unknown
ARG BUILD_TIME=unknown
ENV JAM_BUILD_GIT_SHA=${GIT_SHA}
ENV JAM_BUILD_TIME=${BUILD_TIME}

# Build the assembly JAR (skip native lib build since we already have them)
RUN echo "Building for git=${GIT_SHA} at ${BUILD_TIME}" && \
    sbt clean "conformance/assembly"

# Stage 3: Runtime image
FROM eclipse-temurin:21-jre-jammy

WORKDIR /app

# Install native library dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy native libraries
COPY --from=rust-builder /build/bandersnatch-vrfs-wrapper/target/release/libbandersnatch_vrfs_wrapper.so /app/lib/
COPY --from=rust-builder /build/ed25519-zebra-wrapper/target/release/libed25519_zebra_wrapper.so /app/lib/
COPY --from=rust-builder /build/erasure-coding-wrapper/target/release/liberasure_coding_wrapper.so /app/lib/

# Copy the assembled JAR and production logback config
COPY --from=scala-builder /build/modules/conformance/target/scala-3.3.7/jam-conformance.jar /app/
COPY --from=scala-builder /build/modules/conformance/src/main/resources/logback-prod.xml /app/logback.xml

# Set library path for native libraries
ENV LD_LIBRARY_PATH=/app/lib

ARG GIT_SHA=unknown
ARG BUILD_TIME=unknown
ENV JAM_BUILD_GIT_SHA=${GIT_SHA}
ENV JAM_BUILD_TIME=${BUILD_TIME}
LABEL org.opencontainers.image.revision=${GIT_SHA}
LABEL org.opencontainers.image.created=${BUILD_TIME}

# Set default log level to ERROR for production
ENV LOG_LEVEL=ERROR

ENTRYPOINT ["java", \
    "-XX:+UseZGC", \
    "-XX:+ZGenerational", \
    "-Xms2g", \
    "-Xmx4g", \
    "-XX:+AlwaysPreTouch", \
    "-XX:+UseStringDeduplication", \
    "-Dlogback.configurationFile=/app/logback.xml", \
    "-jar", "/app/jam-conformance.jar"]
