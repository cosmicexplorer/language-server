package scala.meta.internal.builds
import scala.meta.io.AbsolutePath
import scala.concurrent.Future
import scala.meta.internal.metals.BloopInstallResult

abstract class BloopPluginBuildTool extends BuildTool {

  def bloopInstall(
      workspace: AbsolutePath,
      systemProcess: List[String] => Future[BloopInstallResult]
  ): Future[BloopInstallResult] =
    systemProcess(args(workspace))

  def args(workspace: AbsolutePath): List[String]
}
