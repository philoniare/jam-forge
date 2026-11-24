import sbt._
import scala.sys.process._

object NativeBuild {

  val PrebuiltEnv = "JAM_NATIVE_PREBUILT"

  private def prebuilt: Boolean =
    sys.env.get(PrebuiltEnv).map(_.trim.toLowerCase).exists(v => v == "1" || v == "true")

  /** Rust sources whose mtime decides whether the cached library is stale. */
  private def cargoSources(rustProjectDir: File): Seq[File] = {
    val cargoTargetPath = (rustProjectDir / "target").getAbsolutePath
    rustProjectDir
      .**("*.rs" | "Cargo.toml" | "Cargo.lock")
      .get()
      .filterNot(_.getAbsolutePath.startsWith(cargoTargetPath))
  }

  def cargoLib(
      rustProjectDir: File,
      targetDir: File,
      libName: String,
      displayName: String,
      isWindows: Boolean
  ): Unit = {
    val targetLib = targetDir / libName
    val sourceLib = rustProjectDir / "target" / "release" / libName

    val sources = cargoSources(rustProjectDir)
    val newestSource = if (sources.isEmpty) 0L else sources.map(_.lastModified()).max
    val isStale = !targetLib.exists() || targetLib.lastModified() < newestSource
    val cargoMissing =
      try { (if (isWindows) "where cargo" else "which cargo").!(ProcessLogger(_ => ())) != 0 }
      catch { case _: Exception => true }

    if (isStale && targetLib.exists() && cargoMissing) {
      if (prebuilt)
        println(s"$displayName is older than its sources; $PrebuiltEnv is set, using ${targetLib.absolutePath}")
      else
        sys.error(
          s"$displayName at ${targetLib.absolutePath} is older than its sources in ${rustProjectDir.absolutePath} " +
            s"and cargo is not installed to rebuild it. Install cargo, or set $PrebuiltEnv=1 if the library was " +
            "built from these sources elsewhere."
        )
    } else if (isStale) {
      val why = if (targetLib.exists()) "out of date" else "missing"
      println(s"Building $displayName for ${targetDir.getName} ($why)...")
      targetDir.mkdirs()

      val cargoPath =
        try {
          if (isWindows) "where cargo".!!.trim else "which cargo".!!.trim
        } catch { case _: Exception => "cargo" }

      val buildResult = Process(Seq(cargoPath, "build", "--release", "--locked"), rustProjectDir).!
      if (buildResult != 0) {
        sys.error(s"Failed to build $displayName with cargo build --release --locked")
      }

      if (sourceLib.exists()) {
        IO.copyFile(sourceLib, targetLib)
        targetLib.setLastModified(System.currentTimeMillis())
        if (!isWindows) {
          s"chmod +x ${targetLib.absolutePath}".!
        }
        println(s"$displayName built: ${targetLib.absolutePath}")
      } else {
        sys.error(s"$displayName not found at: ${sourceLib.absolutePath}")
      }
    } else {
      println(s"$displayName up to date: ${targetLib.absolutePath}")
    }
  }
}
