package scala.meta.internal.builds

import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.{MetalsServerConfig, UserConfiguration}

case class PantsBuildTool() extends BuildTool {

  override def toString(): String = "pants"
  def version: String = "1.0.0"
  def minimumVersion: String = "1.0.0"
  def recommendedVersion: String = "1.0.0"
  def executableName: String = "pants"
  def digest(
      workspace: AbsolutePath,
      userConfig: UserConfiguration
  ): Option[String] = {
    PantsDigest.current(workspace, userConfig)
  }
  def args(
      workspace: AbsolutePath,
      userConfig: () => UserConfiguration,
      config: MetalsServerConfig
  ): List[String] = {

//    TODO: This command should run in any repository. Might have to add another user configuration to add optional arguments to pants command.
    List(
      workspace.resolve("pants").toString(),
      "compile.rsc",
      "--empty-compilation",
      "--cache-ignore",
      "--no-use-classpath-jars",
      "bloop.bloop-export-config",
      "--sources",
      "bloop.bloop-gen",
      "--execution-strategy=subprocess",
      "compile.errorprone",
      "--skip"
    ) ++ userConfig().pantsTargets
      .getOrElse("::/")
      .split(" ")
      .map(_.trim)
      .toList
    // TODO: remove this, '--pants-config-files=pants.ini.scalameta' is required for source repo.
//     List(
//       workspace.resolve("pants").toString(),
//       "--pants-config-files=pants.ini.scalameta",
//       "compile.rsc",
//       "--empty-compilation",
//       "--cache-ignore",
//       "--no-use-classpath-jars",
//       "bloop.bloop-export-config",
//       "--sources",
//       "bloop.bloop-gen",
//       "--execution-strategy=subprocess"
//     ) ++ userConfig().pantsTargets
//       .getOrElse("::/")
//       .split(" ")
//       .map(_.trim)
//       .toList
  }
}

object PantsBuildTool {
  def isPantsRelatedPath(workspace: AbsolutePath, path: AbsolutePath) = {
    path.toNIO.endsWith("BUILD")
  }
}
