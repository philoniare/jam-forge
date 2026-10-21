# jam-core

Core types, encoding, and primitives for the JAM protocol implementation.

## Overview

The `jam-core` module provides the foundational types and serialization infrastructure for the JAM protocol. This module has no dependencies on other JAM modules and serves as the base layer for the entire implementation.

**Package:** `io.forge.jam.core`

## Key Features

- **Complete JAM Type System**: All protocol types from the Gray Paper
- **Binary Serialization**: JAM-compliant encoding/decoding via type classes
- **Primitive Types**: Hash types, unsigned integers, fixed-size arrays
- **JSON Support**: Test vector parsing and configuration loading
- **Constants**: Protocol constants and configuration values

## Codec System

The codec system provides type-safe binary serialization conforming to the JAM specification (GP 0.8.0 appendix C).
It lives in `io.forge.jam.core.codec` and is a small byte-array library with no third-party dependency.

### Codecs

`Codec[A]` writes into a growable `ByteWriter` and reads from a bounds-checked `ByteReader`, so composed codecs
allocate no intermediate buffers:

```scala
import io.forge.jam.core.codec.{ByteReader, ByteWriter, Codec}

trait Codec[A]:
  def write(out: ByteWriter, value: A): Unit
  def read(in: ByteReader): A

  def encode(value: A): Array[Byte]
  def decode(bytes: Array[Byte]): Either[String, A]                 // ignores trailing bytes
  def decodeExact(bytes: Array[Byte]): Either[String, A]            // rejects trailing bytes
  def decodeAt(bytes: Array[Byte], offset: Int): Either[String, (A, Int)]  // value and bytes consumed
```

Inside a codec, malformed input throws `CodecDecodingException`; the `decode*` entry points turn it into `Left`.

### Composition

Fields compose with `::` into tuples, mapped onto case classes with `xmap` or `as`:

```scala
import io.forge.jam.core.codec.codecs.*

given Codec[Block] = (Codec[Header] :: Codec[Extrinsic]).as[Block]
```

Hot per-block state components write their fields directly with `Codec.of((w, v) => ..., r => ...)`.

### JAM Codecs

`codecs` holds the primitives (`uint8`, `uint16L`, `uint32L`, `int64L`, `compactInt`, `compactBytes`, `bytes(n)`,
`option`, `fixedSizeList`, `compactPrefixedList`, ...) and `JamCodecs` the givens for JAM types (`Hash`, keys,
`UInt`, ...). The compact natural encoding (eq. C.6) is implemented once, in `ByteWriter.compact` and
`ByteReader.compact`, and rejects non-minimal encodings on decode.

## Primitive Types

### Hash Types

```scala
type Hash = Array[Byte]  // 32 bytes
type HashShort = Array[Byte]  // 28 bytes (Bandersnatch public key)
```

### Unsigned Integers

Uses Spire library:
```scala
import spire.math.{UByte, UShort, UInt, ULong}

val u8: UByte = UByte(255)
val u16: UShort = UShort(65535)
val u32: UInt = UInt(4294967295L)
val u64: ULong = ULong("18446744073709551615")
```

### Fixed-Size Types

```scala
type ServiceId = UInt        // 4 bytes
type CoreIndex = UShort      // 2 bytes
type Gas = ULong             // 8 bytes
type Balance = ULong         // 8 bytes
type TimeSlot = UInt         // 4 bytes
```