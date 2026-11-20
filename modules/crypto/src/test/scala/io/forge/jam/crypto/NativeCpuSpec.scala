package io.forge.jam.crypto

import io.forge.jam.vrfs.NativeCpu
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

class NativeCpuSpec extends AnyFunSuite with Matchers:

  private val v3Flags =
    "fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall nx " +
      "mmxext fxsr_opt pdpe1gb rdtscp lm rep_good nopl cpuid extd_apicid tsc_known_freq pni pclmulqdq ssse3 fma cx16 " +
      "pcid sse4_1 sse4_2 movbe popcnt aes xsave avx f16c rdrand hypervisor lahf_lm cmp_legacy cr8_legacy abm sse4a " +
      "misalignsse 3dnowprefetch osvw topoext vmmcall fsgsbase bmi1 avx2 smep bmi2 erms invpcid rdseed adx smap"

  private def cpuinfo(flags: String): String =
    s"processor\t: 0\nvendor_id\t: AuthenticAMD\nmodel name\t: AMD EPYC 7763 64-Core Processor\nflags\t\t: $flags\nbogomips\t: 4890.85\n\n" +
      s"processor\t: 1\nflags\t\t: $flags\n"

  private def without(names: String*): String = v3Flags.split(' ').filterNot(names.contains).mkString(" ")

  private def missing(os: String, arch: String, info: String): List[String] =
    NativeCpu.missingFeatures(os, arch, info).asScala.toList

  test("a Linux x86-64 CPU with every x86-64-v3 instruction set lacks nothing") {
    missing("Linux", "amd64", cpuinfo(v3Flags)) shouldBe Nil
    missing("Linux", "x86_64", cpuinfo(v3Flags)) shouldBe Nil
  }

  test("a Linux x86-64 CPU without AVX2 and BMI2 is said to lack them") {
    missing("Linux", "amd64", cpuinfo(without("avx2", "bmi2"))) shouldBe List("avx2", "bmi2")
    missing("Linux", "amd64", cpuinfo(without("fma", "movbe", "abm", "f16c", "bmi1", "avx"))) shouldBe
      List("avx", "bmi1", "f16c", "fma", "abm", "movbe")
  }

  test("a flag is a whole word: avx2 does not stand for avx, nor bmi1 for bmi2") {
    missing("Linux", "amd64", cpuinfo(without("avx"))) shouldBe List("avx")
    missing("Linux", "amd64", cpuinfo(without("bmi2"))) shouldBe List("bmi2")
  }

  test("another architecture or operating system is not checked, nor a cpuinfo that lists no flags") {
    missing("Linux", "aarch64", "processor\t: 0\nFeatures\t: fp asimd evtstrm aes\n") shouldBe Nil
    missing("Mac OS X", "x86_64", "") shouldBe Nil
    missing("Linux", "amd64", "") shouldBe Nil
    missing("Linux", "amd64", "processor\t: 0\nmodel name\t: unknown\n") shouldBe Nil
  }

  test("the check refuses such a CPU by the names of what it lacks, and says how to run without the check") {
    val refused = the[UnsupportedOperationException] thrownBy NativeCpu.check("Linux", "amd64", cpuinfo(without("avx2", "bmi2")))
    refused.getMessage should include("avx2, bmi2")
    refused.getMessage should include("x86-64-v3")
    refused.getMessage should include(NativeCpu.SKIP_PROPERTY)
    NativeCpu.check("Linux", "amd64", cpuinfo(v3Flags))
    NativeCpu.check("Linux", "aarch64", "")
  }
