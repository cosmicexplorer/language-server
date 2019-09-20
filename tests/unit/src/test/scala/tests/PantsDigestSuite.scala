package tests

import scala.meta.internal.builds.PantsDigest
import scala.meta.internal.metals.UserConfiguration
import scala.meta.io.AbsolutePath

object PantsDigestSuite extends BaseDigestSuite {
  override def digestCurrent(
      root: AbsolutePath
  ): Option[String] = {
    val userConfig = new UserConfiguration(
      pantsTargets = Option("::")
    )
    val fakeDigest: PantsDigest = new PantsDigest(() => userConfig) {
      override protected def getPantsFileDependencies(
          workspace: AbsolutePath,
          pantsTargets: String
      ): String = {
        workspace.resolve("BUILD").toString()
      }
    }
    fakeDigest.current(root)
  }

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
