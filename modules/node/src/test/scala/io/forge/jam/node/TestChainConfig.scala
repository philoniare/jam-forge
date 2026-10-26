package io.forge.jam.node

import io.forge.jam.core.ChainConfig

/** The node specs run on the tiny config; header encoding takes its codec from this. */
given tinyChainConfig: ChainConfig = ChainConfig.TINY
