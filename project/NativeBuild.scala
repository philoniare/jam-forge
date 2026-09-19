import sbt._
import scala.sys.process._

object NativeBuild {

  def cargoLib(
      rustProjectDir: File,
      targetDir: File,
      libName: String,
      displayName: String,
      isWindows: Boolean
  ): Unit = {
    val targetLib = targetDir / libName
    val sourceLib = rustProjectDir / "target" / "release" / libName

    if (!targetLib.exists()) {
      println(s"Building $displayName for ${targetDir.getName}...")
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
        if (!isWindows) {
          s"chmod +x ${targetLib.absolutePath}".!
        }
        println(s"$displayName built: ${targetLib.absolutePath}")
      } else {
        sys.error(s"$displayName not found at: ${sourceLib.absolutePath}")
      }
    } else {
      println(s"$displayName already exists: ${targetLib.absolutePath}")
    }
  }
}
