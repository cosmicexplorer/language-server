package tests.pants

import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.concurrent.TimeUnit
import scala.meta.internal.builds.{PantsBuildTool, PantsDigest}
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.metals.Messages._
import scala.meta.internal.metals.{
  MetalsSlowTaskResult,
  ServerCommands,
  UserConfiguration
}
import scala.meta.io.AbsolutePath
import scala.sys.process._
import tests.BaseImportSuite

object PantsSlowSuite extends BaseImportSuite("pants") {

  val buildTool = PantsBuildTool()
  val src_path = "src/scala/org/pantsbuild/pants-metals"

  def exec(cmd: String): String =
    execAt(cmd, Some(workspace.toFile))

  def execAt(cmd: String, cwd: Option[File]): String = {
    val args: List[String] = cmd.split(" ").toList
    Process(args, cwd).!!
  }

  private def preInitialized = {

    val pants_targets_config =
      s"""
         |{
         |  "pants-targets": "$src_path/::"
         |}
         |""".stripMargin
    server.didChangeConfiguration(pants_targets_config)

  }

  override def utestBeforeEach(path: Seq[String]): Unit = {
    super.utestBeforeEach(path)
    cleanWorkspace()
    initialize()
    // TODO: remove
    exec("rm -rf .metals .bloop idk.json src/scala/org/pantsbuild/pants-metals")
  }

  def initialize() {

    if (!workspace.resolve(".git").isDirectory) {
      exec("git clone https://github.com/pantsbuild/pants.git .")
    }
    // Process(List("git", "pull"), Some(workspace.toFile)).!!

    val branch = "pull-7961"
    val current_branch = exec("git rev-parse --abbrev-ref HEAD")
    // TODO: Temporary changes till pants bloop gen code is merged to master.
    if (current_branch.trim.equals(branch)) {
      Process(List("git", "clean", "-fd"), Some(workspace.toFile)).!!
      Process(List("git", "checkout", "."), Some(workspace.toFile)).!!
      pprint.log("Cleaned branch")
    } else {
      pprint.log("Fetching `pull/7961/head` branch")
      Process(
        List("git", "fetch", "origin", "pull/7961/head:" + branch),
        Some(workspace.toFile)
      ).!!
      Process(List("git", "checkout", "pull-7961"), Some(workspace.toFile)).!!
      Process(List("git", "checkout", "."), Some(workspace.toFile)).!!
    }
    Process(
      List(
        workspace.resolve("pants").toString(),
        "binary",
        "contrib/bloop/src/scala/pants/contrib/bloop/config:bloop-config-gen"
      ),
      Some(workspace.toFile),
      "MODE" -> "debug"
    ).!!
    val bloop_config_jar_path = workspace.resolve("dist/bloop-config-gen.jar")
    // We won't need this once pr 7961 is merged
    Files.write(
      workspace.resolve("BUILD.tools").toNIO,
      s"""
jar_library(
  name = 'bloop-config-gen',
  jars = [
    jar(
      org='org.pantsbuild', name='bloop-config-gen_2.12', rev='???',
      url='file://$bloop_config_jar_path',
      mutable=True)
  ],
)
""".getBytes(),
      StandardOpenOption.APPEND
    )
  }

  override def currentDigest(
      workspace: AbsolutePath
  ): Option[String] = {
    val userconfig = new UserConfiguration(
      pantsTargets = Option(s"$src_path::")
    )
    PantsDigest.current(workspace, userconfig)
  }

  testAsync("basic") {
    for {

      _ <- server.initialize(
        s"""
           |/$src_path/util/BUILD
           |scala_library(
           |  name='math',
           |  sources=globs('*.scala'),
           |)
           |/$src_path/util/Math.scala
           |package math_util
           |class Math {
           |  def add(a: Int, b: Int): Unit = {
           |    a + b
           |  }
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
      _ = client.messageRequests.clear() // restart
      _ = assertStatus(_.isInstalled)
      _ = assertNoDiff(client.workspaceMessageRequests, "")
      _ <- server.didChange(s"$src_path/util/BUILD") { text =>
        s"""${text.replace("math", "math1")}
           |
           |""".stripMargin
      }
      _ = assertNoDiff(client.workspaceMessageRequests, "")
      _ <- server.didSave(s"$src_path/util/BUILD")(identity)
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

  testAsync("force-command") {
    for {
      _ <- server.initialize(
        s"""|/$src_path/util/BUILD
            |scala_library(
            |  name='math',
            |  sources=globs('*.scala'),
            |)
            |/$src_path/util/Math.scala
            |package math_util
            |class Math {
            |  def add(a: Int, b: Int): Unit = {
            |    a + b
            |  }
            |}
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
      _ = client.messageRequests.clear() // restart
      _ <- server.executeCommand(ServerCommands.ImportBuild.id)
      _ = assertNoDiff(
        client.workspaceMessageRequests,
        List(
          progressMessage
        ).mkString("\n")
      )
    } yield ()
  }

  testAsync("cancel") {
    exec("rm -rf .metals .bloop idk.json src/scala/org/pantsbuild/pants-metals")
    client.slowTaskHandler = params => {
      if (params == bloopInstallProgress("pants")) {
        Thread.sleep(TimeUnit.SECONDS.toMillis(2))
        Some(MetalsSlowTaskResult(cancel = true))
      } else {
        None
      }
    }
    for {
      _ <- server.initialize(
        s"""|/$src_path/util/BUILD
            |scala_library(
            |  name='math',
            |  sources=globs('*.scala'),
            |)
            |/$src_path/util/Math.scala
            |package math_util
            |class Math {
            |  def add(a: Int, b: Int): Unit = {
            |    a + b
            |  }
            |}
            |""".stripMargin,
        expectError = true,
        preInitialized = () => preInitialized
      )
      _ = assertStatus(!_.isInstalled)
      _ = client.slowTaskHandler = _ => None
      _ = assertStatus(!_.isInstalled)
      _ <- server.didSave(s"$src_path/util/BUILD")({ text =>
        s"""$text
           |
          """.stripMargin
      })
      _ = assertNoDiff(client.workspaceShowMessages, "")
      _ = assertStatus(_.isInstalled)
    } yield ()
  }

  testAsync("transitive") {
    for {
      _ <- server.initialize(
        s"""|/$src_path/util/BUILD
            |scala_library(
            |  name='math',
            |  sources=globs('*.scala'),
            |)
            |/$src_path/util/Math.scala
            |package util
            |class Math {
            |  def add(a: Int, b: Int): Unit = {
            |    a + b
            |  }
            |}
            |
            |/$src_path/main/BUILD
            |scala_library(
            |  name='main',
            |  sources=globs('*.scala'),
            |  dependencies = [
            |    "$src_path/util:math"
            |  ]
            |)
            |/$src_path/main/Main.scala
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
    exec("rm -rf .metals .bloop idk.json src/scala/org/pantsbuild/pants-metals")
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
           |/$src_path/warning/BUILD
           |scala_library(
           |  name='warning',
           |  sources=globs('*.scala'),
           |)
           |/$src_path/warning/Warning.scala
           |import scala.concurrent.Future // unused
           |object Warning
           |""".stripMargin,
        preInitialized = () => preInitialized
      )
      _ = assertStatus(_.isInstalled)
      _ <- server.didOpen(s"$src_path/warning/Warning.scala")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        s"""
           |$src_path/warning/Warning.scala:1:1: error: Unused import
           |import scala.concurrent.Future // unused
           |^^^^^^^
        """.stripMargin
      )
    } yield ()
  }

  testAsync("new-dependency") {
    for {
      _ <- server.initialize(
        s"""|/$src_path/util/BUILD
            |scala_library(
            |  name='math',
            |  sources=globs('*.scala'),
            |)
            |/$src_path/util/Math.scala
            |package util
            |class Math {
            |  def add(a: Int, b: Int): Unit = {
            |    a + b
            |  }
            |}
            |/$src_path/main/Main.scala
            |
            |/$src_path/main/BUILD
            |
            |""".stripMargin,
        preInitialized = () => preInitialized
      )
      _ <- server.didOpen(s"$src_path/main/Main.scala")

      _ <- server.didSave(s"$src_path/main/Main.scala") { text =>
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
      _ <- server.didSave(s"$src_path/main/BUILD") { text =>
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
        .didSave(s"$src_path/main/Main.scala") { text =>
          text.replaceAll("\"", "")
        }
        .recover { case e => scribe.error("compile", e) }
      _ = assertNoDiff(client.workspaceDiagnostics, "")
    } yield ()
  }
}
