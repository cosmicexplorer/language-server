package tests.pants

import java.io.FileInputStream
import java.nio.charset.StandardCharsets
import scala.meta.internal.builds.{PantsBuildTool, PantsDigest}
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.metals.Messages._
import scala.meta.internal.metals.UserConfiguration
import scala.meta.io.AbsolutePath
import tests.BaseImportSuite
import scala.meta.internal.builds.BuildTool

object PantsSlowSuite extends BaseImportSuite("pants") {

  val buildTool = PantsBuildTool(() => userConfig)

  private def preInitialized = {
    val pants_targets_config =
      s"""
         |{
         |  "pants-targets": "src::"
         |}
         |""".stripMargin
    server.didChangeConfiguration(pants_targets_config)
  }

  override def utestBeforeEach(path: Seq[String]): Unit = {
    super.utestBeforeEach(path)
    installPants()
  }

  def installPants(): Unit = {
    cleanWorkspace()
    val pants = BuildTool.copyFromResource(workspace.toNIO, "pants")
    pants.toFile().setExecutable(true)
    import scala.sys.process._
    val exit = List(pants.toString(), "generate-pants-ini").!
    require(exit == 0, "failed to generate pants.ini")
  }

  override def currentDigest(
      workspace: AbsolutePath
  ): Option[String] = {
    new PantsDigest(
      () => UserConfiguration(pantsTargets = Option(s"src::"))
    ).current(workspace)
  }

  testAsync("basic") {
    for {
      _ <- server.initialize(
        s"""
           |/src/BUILD
           |scala_library(
           |  name='math',
           |  sources=globs('*.scala'),
           |)
           |/src/Math.scala
           |package src
           |class Math {
           |  def add(a: Int, b: Int): Unit = a + b
           |}
           |""".stripMargin,
        preInitialized = () => preInitialized
      )
      _ = assertNoDiff(
        client.workspaceMessageRequests,
        List(
          importBuildMessage,
          progressMessage,
          CheckDoctor.allProjectsMisconfigured
        ).mkString("\n")
      )
      _ = client.messageRequests.clear() // restart
      _ = assertStatus(_.isInstalled)
      _ = assertNoDiff(client.workspaceMessageRequests, "")
      _ <- server.didChange(s"src/BUILD") { text =>
        s"""${text.replace("math", "math1")}
           |
           |""".stripMargin
      }
      _ = assertNoDiff(client.workspaceMessageRequests, "")
      _ <- server.didSave(s"src/BUILD")(identity)
    } yield {
      assertNoDiff(
        client.workspaceMessageRequests,
        List(
          // Project has .bloop directory so user is asked to "re-import project"
          importBuildChangesMessage,
          progressMessage
        ).mkString("\n")
      )
      Thread.sleep(1000)
    }
  }

  testAsync("transitive") {
    for {
      _ <- server.initialize(
        s"""|/src/util/BUILD
            |scala_library(
            |  name='math',
            |  sources=globs('*.scala'),
            |)
            |/src/util/Math.scala
            |package util
            |class Math {
            |  def add(a: Int, b: Int): Unit = {
            |    a + b
            |  }
            |}
            |
            |/src/main/BUILD
            |scala_library(
            |  name='main',
            |  sources=globs('*.scala'),
            |  dependencies = [
            |    "src/util:math"
            |  ]
            |)
            |/src/main/Main.scala
            |package main
            |
            |import util.Math
            |
            |object Main {
            |
            |	def main(args: Array[String]) {
            |		val math = new Math
            |		println(math.add(3,6))  // prints (9)
            |	}
            |}
            |
            |""".stripMargin,
        preInitialized = () => preInitialized
      )
      _ = assertNoDiff(
        client.workspaceMessageRequests,
        List(
          // Project has no .bloop directory so user is asked to "import via bloop"
          importBuildMessage,
          progressMessage,
          CheckDoctor.allProjectsMisconfigured
        ).mkString("\n")
      )
      _ = client.messageRequests.clear()
      _ = assertStatus(_.isInstalled)
    } yield assertNoDiff(client.workspaceMessageRequests, "")
  }
//
  testAsync("fatal-warnings") {
    val scala_options = List(
      "'-S-target:jvm-1.8',",
      "'-S-Xfatal-warnings',",
      "'-S-Ywarn-unused',"
    )
    // This adds '-S-Xfatal-warnings' and '-S-Ywarn-unused' options to scala in the pants.ini file
    val pants_ini = new String(
      InputStreamIO.readBytes(
        new FileInputStream(workspace.resolve("pants.ini").toNIO.toString)
      ),
      StandardCharsets.UTF_8
    ).replace("'-S-target:jvm-1.8',", scala_options.mkString("\n\t"))

    for {
      _ <- server.initialize(
        s"""
           |/pants.ini
           |$pants_ini
           |/src/warning/BUILD
           |scala_library(
           |  name='warning',
           |  sources=globs('*.scala'),
           |)
           |/src/warning/Warning.scala
           |import scala.concurrent.Future // unused
           |object Warning
           |""".stripMargin,
        preInitialized = () => preInitialized
      )
      _ = assertStatus(_.isInstalled)
      _ <- server.didOpen(s"src/warning/Warning.scala")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        s"""
           |src/warning/Warning.scala:1:1: error: Unused import
           |import scala.concurrent.Future // unused
           |^^^^^^^
        """.stripMargin
      )
    } yield ()
  }

  testAsync("new-dependency") {
    for {
      _ <- server.initialize(
        s"""|/src/util/BUILD
            |scala_library(
            |  name='math',
            |  sources=globs('*.scala'),
            |)
            |/src/util/Math.scala
            |package util
            |class Math {
            |  def add(a: Int, b: Int): Unit = {
            |    a + b
            |  }
            |}
            |/src/main/Main.scala
            |
            |/src/main/BUILD
            |
            |""".stripMargin,
        preInitialized = () => preInitialized
      )
      _ <- server.didOpen(s"src/main/Main.scala")

      _ <- server.didSave(s"src/main/Main.scala") { text =>
        """|package main
           |
           |import util.Math
           |
           |object Main {
           |
           |	def main(args: Array[String]) {
           |		val math = new Math
           |		println("math.add(3,6)")  // prints (9)
           |	}
           |}
           |
           |""".stripMargin
      }
      _ = assertNoDiff(client.workspaceDiagnostics, "")
      _ <- server.didSave(s"src/main/BUILD") { text =>
        s"""|
            |jvm_binary(
            |  name='main',
            |  main='src.scala.org.pantsbuild.pants-metals.main.Main',
            |  sources=globs('*.scala'),
            |  dependencies = [
            |    "src/scala/org/pantsbuild/pants-metals/util:math"
            |  ]
            |)
            |""".stripMargin
      }
      _ <- server
        .didSave(s"src/Main.scala") { text =>
          text.replaceAll("\"", "")
        }
        .recover { case e => scribe.error("compile", e) }
      _ = assertNoDiff(client.workspaceDiagnostics, "")
    } yield ()
  }
}
