import sbt._
import scala.sys.process._

object NativeBuild {

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

    if (isStale) {
      val why = if (targetLib.exists()) "out of date" else "missing"
      println(s"Building $displayName for ${targetDir.getName} ($why)...")
      targetDir.mkdirs()

      val cargoPath =
        try {
          if (isWindows) "where cargo".!!.trim else "which cargo".!!.trim
        } catch { case _: Exception => "cargo" }

      val buildResult = Process(Seq(cargoPath, "build", "--release"), rustProjectDir).!
      if (buildResult != 0) {
        sys.error(s"Failed to build $displayName with cargo")
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
