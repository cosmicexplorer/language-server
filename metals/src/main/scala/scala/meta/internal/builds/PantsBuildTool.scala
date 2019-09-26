package scala.meta.internal.builds

import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.pantsbuild.BloopPants
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.meta.internal.metals.BloopInstallResult
import scala.meta.internal.metals.Timer
import scala.meta.internal.metals.Time
import scala.util.Failure
import scala.util.Success

case class PantsBuildTool(userConfig: () => UserConfiguration)(
    implicit ec: ExecutionContext
) extends BuildTool {
  override def toString(): String = "pants"
  def version: String = "1.0.0"
  def minimumVersion: String = "1.0.0"
  def recommendedVersion: String = "1.0.0"
  def executableName: String = "pants"
  def digest(workspace: AbsolutePath): Option[String] = {
    new PantsDigest(userConfig).current(workspace)
  }

  def bloopInstall(
      workspace: AbsolutePath,
      systemProcess: List[String] => Future[BloopInstallResult]
  ): Future[BloopInstallResult] = {
    userConfig().pantsTargets match {
      case None =>
        Future.successful(BloopInstallResult.Failed(1))
      case Some(targets) =>
        Future {
          val timer = new Timer(Time.system)
          BloopPants.bloopInstall(workspace.toNIO, targets, isCached = false) match {
            case Failure(error) =>
              scribe.error(s"pants bloopInstall failed: $error")
              BloopInstallResult.Failed(1)
            case Success(count) =>
              scribe.info(s"Exported ${count} Pants targets(s) in $timer")
              BloopInstallResult.Installed
          }
        }
    }
  }
}

object PantsBuildTool {
  def isPantsRelatedPath(workspace: AbsolutePath, path: AbsolutePath) = {
    path.toNIO.endsWith("BUILD")
  }
}
