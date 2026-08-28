package io.forge.jam.network

/** JAMNP-S stream kinds. UP (unique persistent) kinds number from 0 and live
  * for the connection's duration, opened by the connection initiator; CE
  * (common ephemeral) kinds number from 128 and are opened per
  * request/submission and closed on completion.
  */
object StreamKind:
  // UP
  val BlockAnnouncement: Byte = 0

  // CE
  val BlockRequest: Byte = 128.toByte
  val StateRequest: Byte = 129.toByte
  val TicketDistributionStep1: Byte = 131.toByte
  val TicketDistributionStep2: Byte = 132.toByte
  val WorkPackageSubmission: Byte = 133.toByte
  val WorkPackageSharing: Byte = 134.toByte
  val WorkReportDistribution: Byte = 135.toByte
  val WorkReportRequest: Byte = 136.toByte
  val ShardDistribution: Byte = 137.toByte
  val AuditShardRequest: Byte = 138.toByte
  val SegmentShardRequest: Byte = 139.toByte
  val SegmentShardRequestVerified: Byte = 140.toByte
  val AssuranceDistribution: Byte = 141.toByte
  val PreimageAnnouncement: Byte = 142.toByte
  val PreimageRequest: Byte = 143.toByte
  val AuditAnnouncement: Byte = 144.toByte
  val JudgmentPublication: Byte = 145.toByte
  val WorkPackageBundleSubmission: Byte = 146.toByte
  val BundleRequest: Byte = 147.toByte
  val SegmentRequest: Byte = 148.toByte

  inline def isUp(kind: Byte): Boolean = (kind & 0x80) == 0
  inline def isCe(kind: Byte): Boolean = (kind & 0x80) != 0

  def name(kind: Byte): String = kind match
    case BlockAnnouncement           => "UP0/block-announcement"
    case BlockRequest                => "CE128/block-request"
    case StateRequest                => "CE129/state-request"
    case TicketDistributionStep1     => "CE131/ticket-distribution-1"
    case TicketDistributionStep2     => "CE132/ticket-distribution-2"
    case WorkPackageSubmission       => "CE133/work-package-submission"
    case WorkPackageSharing          => "CE134/work-package-sharing"
    case WorkReportDistribution      => "CE135/work-report-distribution"
    case WorkReportRequest           => "CE136/work-report-request"
    case ShardDistribution           => "CE137/shard-distribution"
    case AuditShardRequest           => "CE138/audit-shard-request"
    case SegmentShardRequest         => "CE139/segment-shard-request"
    case SegmentShardRequestVerified => "CE140/segment-shard-request-verified"
    case AssuranceDistribution       => "CE141/assurance-distribution"
    case PreimageAnnouncement        => "CE142/preimage-announcement"
    case PreimageRequest             => "CE143/preimage-request"
    case AuditAnnouncement           => "CE144/audit-announcement"
    case JudgmentPublication         => "CE145/judgment-publication"
    case WorkPackageBundleSubmission => "CE146/work-package-bundle-submission"
    case BundleRequest               => "CE147/bundle-request"
    case SegmentRequest              => "CE148/segment-request"
    case other                       => s"unknown($other)"

/** The preferred-initiator rule (jamnp-s spec): between validators with
  * Ed25519 keys a and b, P(a, b) determines which side should initiate the
  * connection: a when (a31 > 127) xor (b31 > 127) xor (a < b), else b.
  */
object PreferredInitiator:
  def of(a: Array[Byte], b: Array[Byte]): Array[Byte] =
    require(a.length == 32 && b.length == 32)
    val aHigh = (a(31) & 0xff) > 127
    val bHigh = (b(31) & 0xff) > 127
    val aLess = lexLess(a, b)
    if aHigh ^ bHigh ^ aLess then a else b

  private def lexLess(a: Array[Byte], b: Array[Byte]): Boolean =
    var i = 0
    while i < 32 do
      val ai = a(i) & 0xff
      val bi = b(i) & 0xff
      if ai != bi then return ai < bi
      i += 1
    false
