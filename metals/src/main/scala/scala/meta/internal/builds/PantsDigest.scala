package scala.meta.internal.builds

import scala.meta.io.AbsolutePath
import java.security.MessageDigest
import scala.sys.process._

import scala.meta.internal.metals.UserConfiguration

trait PantsDigestTrait extends Digestable {
  override protected def digestWorkspace(
      workspace: AbsolutePath,
      digest: MessageDigest,
      userConfig: UserConfiguration
  ): Boolean = {
    userConfig.pantsTargets match {
      case None => false
      case Some(pantsTargets) =>
        hasBUILDfilesChanged(workspace, digest, pantsTargets)
    }
  }

  def hasBUILDfilesChanged(
      workspace: AbsolutePath,
      digest: MessageDigest,
      pantsTargets: String
  ): Boolean = {
    val pantsFileDeps: String =
      getPantsFileDependencies(workspace, pantsTargets)
    pantsFileDeps.linesIterator
      .map { file =>
        java.nio.file.Paths.get(file).toAbsolutePath.normalize
      }
      .filter(_.endsWith("BUILD"))
      .forall(file => Digest.digestFile(AbsolutePath(file), digest))
  }

  protected def getPantsFileDependencies(
      workspace: AbsolutePath,
      pantsTargets: String
  ) = {
    val args = List(
      workspace.resolve("pants").toString(),
      "filedeps"
    ) ++ pantsTargets.split(" ").map(_.trim).toList
    val pantsFileDeps = Process(args, Some(workspace.toFile)).!!.trim
    pantsFileDeps
  }
}

object PantsDigest extends PantsDigestTrait
