import io.forge.jam.vrfs.RustLibrary
import io.forge.jam.vrfs.use
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class RustLibraryTest {
    @Test
    fun testRingVrfCommitment() {
        val ringSize = 6
        val proverKeyIndex = 3
        val publicKeysHex = listOf(
            "0xaa2b95f7572875b0d0f186552ae745ba8222fc0b5bd456554bfe51c68938f8bc",
            "0xf16e5352840afb47e206b5c89f560f2611835855cf2e6ebad1acc9520a72591d",
            "0x5e465beb01dbafe160ce8216047f2155dd0569f058afd52dcea601025a8d161d",
            "0x48e5fcdce10e0b64ec4eebd0d9211c7bac2f27ce54bca6f7776ff6fee86ab3e3",
            "0x3d5e5a51aab2b048f8686ecd79712a80e3265a114cc73f14bdb2a59233fb66d0",
            "0x7f6190116d118d643a98878e294ccf62b509e214299931aad8ff9764181a4e33"
        )

        val publicKeys = publicKeysHex.map { hex ->
            hex.removePrefix("0x").chunked(2).map { it.toInt(16).toByte() }.toByteArray()
        }

        RustLibrary.use(publicKeys, ringSize, proverKeyIndex) { (_, verifierPtr) ->
            val commitmentBytes = RustLibrary.getVerifierCommitment(verifierPtr)
            assertNotNull(commitmentBytes, "Commitment should not be null")

            val commitmentHex = commitmentBytes.joinToString("") { "%02x".format(it) }

            val expectedGammaZ =
                "95f318fbd93287e8c3987874cded9b29adae70cf109c4321636fd9faeea003f8140710df8894ffbd6c84eaef4ff7cb58b17892749bb3cb3efe528eef951c688f7e83d3ede96f432de64f0b07e5c3cf0232f79ea4c221b7407f9a2348c0a0110692e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

            assertEquals(expectedGammaZ, commitmentHex, "Commitment does not match expected value")
        }
    }
}
