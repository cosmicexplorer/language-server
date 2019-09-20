package scala.meta.internal.builds

import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.metals.JavaBinary
import java.io.File
import scala.meta.internal.mtags.ClasspathLoader
import java.nio.file.Paths

case class PantsBuildTool(userConfig: () => UserConfiguration)
    extends BuildTool {
  override def toString(): String = "pants"
  def version: String = "1.0.0"
  def minimumVersion: String = "1.0.0"
  def recommendedVersion: String = "1.0.0"
  def executableName: String = "pants"
  def digest(workspace: AbsolutePath): Option[String] = {
    new PantsDigest(userConfig).current(workspace)
  }
  def args(workspace: AbsolutePath): List[String] = {
    userConfig().pantsTargets match {
      case None =>
        List(
          "echo",
          "the 'pants-target' setting must be defined."
        )
      case Some(targets) =>
        val classpath = ClasspathLoader
          .getURLs(this.getClass.getClassLoader)
          .map(url => Paths.get(url.toURI))
          .filter(path => {
            val filename = path.toString()
            // Slim down the classpath so the console output is not too crazy. In the future,
            // it would be nice to call `./pants bloop-install` instead.
            filename.contains("scala-library") ||
            filename.contains("scala-collection-compat") ||
            filename.contains("coursier") ||
            filename.contains("cats") ||
            filename.contains("ujson") ||
            filename.contains("upickle") ||
            filename.contains("bloop") ||
            filename.contains("circe") ||
            filename.contains("metals")
          })
          .mkString(File.pathSeparator)
        List(
          JavaBinary(None),
          "-classpath",
          classpath,
          "scala.meta.internal.pantsbuild.BloopPants",
          workspace.toString(),
          targets
        )
    }
  }
}

object PantsBuildTool {
  def isPantsRelatedPath(workspace: AbsolutePath, path: AbsolutePath) = {
    path.toNIO.endsWith("BUILD")
  }
}
