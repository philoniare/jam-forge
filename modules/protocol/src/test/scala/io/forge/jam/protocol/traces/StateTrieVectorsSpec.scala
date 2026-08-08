package io.forge.jam.protocol.traces

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.parser.parse
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{StateTrie, InMemoryTrieBackend}

import scala.io.Source

/**
 * Executes the official W3F state-trie test vectors (jamtestvectors/trie/trie.json)
 */
class StateTrieVectorsSpec extends AnyFlatSpec with Matchers:

  private val vectorsPath = "jamtestvectors/trie/trie.json"

  private final case class TrieCase(index: Int, kv: Map[JamBytes, JamBytes], expectedRoot: Hash)

  private def loadCases(): List[TrieCase] =
    val raw =
      val source = Source.fromFile(vectorsPath)
      try source.mkString
      finally source.close()

    val json = parse(raw).fold(err => fail(s"Failed to parse $vectorsPath: $err"), identity)
    val arr = json.asArray.getOrElse(fail(s"$vectorsPath: expected a JSON array at the top level"))

    arr.toList.zipWithIndex.map { (caseJson, idx) =>
      val obj = caseJson.asObject.getOrElse(fail(s"case $idx: expected a JSON object"))
      val inputObj = obj("input").flatMap(_.asObject).getOrElse(fail(s"case $idx: missing 'input' object"))
      val outputHex = obj("output").flatMap(_.asString).getOrElse(fail(s"case $idx: missing 'output' string"))

      val kv: Map[JamBytes, JamBytes] = inputObj.toMap.map { (keyHex, valueJson) =>
        val valueHex = valueJson.asString.getOrElse(fail(s"case $idx: value for key $keyHex is not a string"))
        val fullKey = JamBytes.fromHexUnsafe(keyHex)
        // Leaf encoding embeds only the first 31 bytes of the state key (blob[31]).
        val key31 = fullKey.take(31)
        key31 -> JamBytes.fromHexUnsafe(valueHex)
      }

      TrieCase(idx, kv, Hash.fromHex(outputHex).fold(err => fail(s"case $idx: bad output hash: $err"), identity))
    }

  private def stateTrieRootOf(kv: Map[JamBytes, JamBytes]): Hash =
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    trie.update(kv.toSeq.map((k, v) => (k, Some(v): Option[JamBytes])))
    trie.rootHash

  private val cases: List[TrieCase] = loadCases()

  "StateTrie" should s"reproduce every official jamtestvectors/trie/trie.json root (${cases.size} cases)" in {
    cases.size should be > 0
    cases.foreach { c =>
      withClue(s"StateTrie case ${c.index} (${c.kv.size} keys), expected root ${c.expectedRoot.toHex}: ") {
        stateTrieRootOf(c.kv) shouldBe c.expectedRoot
      }
    }
  }

  "StateMerklization" should s"reproduce every official jamtestvectors/trie/trie.json root (${cases.size} cases)" in {
    cases.size should be > 0
    cases.foreach { c =>
      withClue(s"StateMerklization case ${c.index} (${c.kv.size} keys), expected root ${c.expectedRoot.toHex}: ") {
        StateMerklization.stateMerklize(c.kv) shouldBe c.expectedRoot
      }
    }
  }
