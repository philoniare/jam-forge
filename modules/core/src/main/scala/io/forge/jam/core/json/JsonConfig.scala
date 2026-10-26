package io.forge.jam.core.json

import io.circe.derivation.Configuration

/** Test vectors name fields in snake_case; derived decoders map them to the camelCase case-class fields. */
object JsonConfig:
  given Configuration = Configuration.default.withSnakeCaseMemberNames
