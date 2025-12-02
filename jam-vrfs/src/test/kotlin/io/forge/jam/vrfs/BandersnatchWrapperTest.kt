import io.forge.jam.vrfs.BandersnatchWrapper
import org.junit.jupiter.api.Disabled
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class RustLibraryTest {
    @Test
    fun testRingVrfCommitment() {
        val ringSize = 6
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

        val bandersnatchWrapper = BandersnatchWrapper(ringSize)
        val commitmentBytes = bandersnatchWrapper.generateRingRoot(publicKeys, ringSize)
        assertNotNull(commitmentBytes, "Commitment should not be null")

        val commitmentHex = commitmentBytes.joinToString("") { "%02x".format(it) }

        val expectedGammaZ =
            "8387a131593447e4e1c3d4e220c322e42d33207fa77cd0fedb39fc3491479ca47a2d82295252e278fa3eec78185982ed82ae0c8fd691335e703d663fb5be02b3def15380789320636b2479beab5a03ccb3f0909ffea59d859fcdc7e187e45a8c92e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

        assertEquals(expectedGammaZ, commitmentHex, "Commitment does not match expected value")
    }

    @Test
    @Disabled("Signature was generated with deprecated ark-ec-vrfs library")
    fun testRingVrfVerify() {
        val ringSize = 6
        val signatureHex =
            "1dfb7b61deee0c4a6899c1123e9e362f2b965079be576aebda0b7ac5e111186e45649b3aa58e18cb4faa3cc74688a322fcd5ab4d591dc2e183f4e31f3f5b926fefcb067f0b43fc9bda32af4bdcbf8767945d01e9816327857a3537929a304eea39fa798e7a8327dcba940ffa77659b1850f877be7a439fc3a66191299d5ae9240e005d84cfbd9fe9a1b250873d484fdf90a03e6b3238a3143f3692ef3128cf1961c8246ca93c957ea7907f7d1dd80e745ecada3f48dbdcbf14dca402df264303a445e67a96d618c4fc1c100a69fa449cca1db6a7ac609d7bd04107f685411aa1bebecdb3897ed0d6c00b46a56381e5968b7520b215677afa1394464057709837dfb22b04b53c69011da7926c341cf6e77a2ed27912c40267c826cca53f876dad8952d2acaa7ebb6da732dd2e34cc2211a953d575e70137e69ce03526f0677ffc2c77f3488b19a3383188e0beaa03fd32b97e7a991fadd8d9d2c71a2d37c836adba04e6f9b5a9f58ecdfb3aaaada4161e283ecb8c295efdf53635ba73ca20f47181e9d7529da701fb654d55d99ff7a9af4c5b678f7ba7d764c48ea2b15569ee1d20a10029a50d5b1100356b64916c6c38526d1dac1f169e5f4250f27dd9cb7454af6591aac3e7353b3a083f310d735eb7a7530fcddc3d06fad7efbca9bf7e3e22306fa2dfd6a184cf5d0acc29ef7cc35c07a7c1e2fc75bb42c80834f000c5d85b293f40ed6b605ec94df44d87712eec2ad1d1386564e5fd2491ec42317aea9058a44e850b08e2a2c4d261c7c59a96be65d89555285c24326c052b5d102cb8a8341b97fcdb28bf8ad1a5591613dc119f7bc44abba455075baf954149a05ae35f13a1e585445577cd9f13c20a04194307716df5f290403147dc728cff8324ee06b0724bbd11fb59a4f3d717a15726c41113e1fd7adcacb5e303a974dd1f6b74028bd1c61f204a43a8ec0bdcf161ff8f2310ac411ceb866d641d29ca3e68fa259f6ee960d3d5835f930f788a3021c5467b619dbc9e5b8b80a6bdac820d1f56fa282fac56c299a7b2cd1d7b2d9b81e6c28bf2a7d6da6b40a02f4ba2aee0f37311c16b941aeaa11e6e40b16f91cad2089ccfc3"
        val signatureBytes = signatureHex.chunked(2).map { it.toInt(16).toByte() }.toByteArray()
        val gammaZ =
            "a949a60ad754d683d398a0fb674a9bbe525ca26b0b0b9c8d79f210291b40d286d9886a9747a4587d497f2700baee229ca72c54ad652e03e74f35f075d0189a40d41e5ee65703beb5d7ae8394da07aecf9056b98c61156714fd1d9982367bee2992e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"
        val gammaZBytes = gammaZ.chunked(2).map { it.toInt(16).toByte() }.toByteArray()
        val entropy = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
        val entropyBytes = entropy.chunked(2).map { it.toInt(16).toByte() }.toByteArray()
        val attempt = 1L

        val bandersnatchWrapper = BandersnatchWrapper(ringSize)
        val result =
            bandersnatchWrapper.verifyRingProof(entropyBytes, attempt, signatureBytes, gammaZBytes, ringSize)
        println("Result: ${result.joinToString("") { "%02x".format(it) }}")
        assert(!result.all { it == 0.toByte() })
    }
}
