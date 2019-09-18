package tests

import scala.meta.internal.builds.PantsDigestTrait
import scala.meta.internal.metals.UserConfiguration
import scala.meta.io.AbsolutePath

object TestPantsDigest extends PantsDigestTrait {

  private var currentFileDeps: String = ""
  def setFileDeps(filedeps: String): Unit = {
    currentFileDeps = filedeps
  }

  override protected def getPantsFileDependencies(
      workspace: AbsolutePath,
      pantsTargets: String
  ) = {
    currentFileDeps
      .split(" ")
      .map(workspace + "/" + _)
      .mkString(" ")
  }
}

object PantsDigestSuite extends BaseDigestSuite {

  TestPantsDigest.setFileDeps("")
  override def digestCurrent(
      root: AbsolutePath
  ): Option[String] = {
    val userConfig = new UserConfiguration(
      pantsTargets = Option("::")
    )
    TestPantsDigest.current(root, userConfig)
  }

  TestPantsDigest.setFileDeps("BUILD")
  checkSame(
    "same-BUILD",
    """
      |/BUILD
      |scala_library(
      |  name='a',
      |  sources=globs('*.scala'),
      |)
    """.stripMargin,
    """
      |/BUILD
      |scala_library(
      |  name='a',
      |  sources=globs('*.scala'),
      |)
    """.stripMargin
  )

  checkDiff(
    "comments-whitespace",
    """
      |/BUILD
      |scala_library(
      |  name='a',
      |  sources=globs('*.scala'),
      |)
    """.stripMargin,
    """
      |/BUILD
      |scala_library(
      |  name='a',
      |  sources=globs(
      |   '*.scala'
      |  ),
      |)
    """.stripMargin
  )

  checkDiff(
    "diff-BUILD",
    """
      |/BUILD
      |java_library(
      |  name='a',
      |  sources=globs('*.java'),
      |)
    """.stripMargin,
    """
      |/BUILD
      |scala_library(
      |  name='a',
      |  sources=globs('*.scala'),
      |)
    """.stripMargin
  )
}
