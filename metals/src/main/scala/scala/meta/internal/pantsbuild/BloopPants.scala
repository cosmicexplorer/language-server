package scala.meta.internal.pantsbuild

import bloop.config.{Config => C}
import java.nio.file.Paths
import scala.sys.process._
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.collection.mutable
import ujson.Value
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.meta.internal.metals.BuildInfo
import scala.meta.internal.metals.Timer
import scala.meta.internal.metals.Time
import java.nio.file.NoSuchFileException
import scala.meta.internal.mtags.MD5

object BloopPants {

  def main(args: Array[String]): Unit = {
    if (args.sameElements(Array("--help")) || args.length != 2) {
      println(
        """pants-bloop <workspacedir> <target>"
          |
          |Command-line tool to export a Pants build into Bloop JSON config files.
          |
          |workpacedir: the base directory of the workspace where pants.ini exists.
          |target: a target to export such as "src/scala::".
          |""".stripMargin
      )
    } else {
      val workspace = Paths.get(args(0))
      val target = args(1)
      val timer = new Timer(Time.system)
      bloopInstall(workspace, target, isCached = false) match {
        case Failure(exception) =>
          scribe.error(s"bloopInstall failed in $timer", exception)
          sys.exit(1)
        case Success(count) =>
          scribe.info(s"Exported ${count} project(s) in $timer")
      }
    }
  }

  def bloopInstall(
      workspace: Path,
      target: String,
      isCached: Boolean
  ): Try[Int] = Try {
    val cacheDir =
      Files.createDirectories(workspace.resolve(".pants.d").resolve("metals"))
    val outputFilename = {
      val processed = target.replaceAll("[^a-zA-Z0-9]", "")
      if (processed.isEmpty()) {
        MD5.compute(target) // necessary for targets like "::/"
      } else {
        processed
      }
    }
    val outputFile = cacheDir.resolve(s"$outputFilename.json")
    val bloopDir = Files.createDirectories(workspace.resolve(".bloop"))

    if (!isCached || !Files.isRegularFile(outputFile)) {
      runPantsExport(workspace, outputFile, target)
    }

    if (Files.isRegularFile(outputFile)) {
      val text =
        new String(Files.readAllBytes(outputFile), StandardCharsets.UTF_8)
      val json = ujson.read(text)
      new BloopPants(workspace, bloopDir, target, json).run()
    } else {
      throw new NoSuchFileException(
        outputFile.toString(),
        null,
        "expected this file to exist after running `./pants export`"
      )
    }
  }

  private def runPantsExport(
      workspace: Path,
      outputFile: Path,
      target: String
  ): Unit = {
    val exportTimer = new Timer(Time.system)
    val pantsBinary = workspace.resolve("pants").toString()
    val commandName = s"$pantsBinary export $target"
    scribe.info(s"running '$commandName', this may take a while...")
    val command = List[String](
      pantsBinary,
      "--export-libraries-sources",
      s"--export-output-file=$outputFile",
      "export",
      target
    )
    val exit = Process(command, Some(workspace.toFile)).!
    if (exit != 0) {
      scribe.info()
      sys.error(s"pants export failed with exit code $exit: $command")
    } else {
      scribe.info(s"time: ran $commandName in $exportTimer")
    }
  }

}

private class BloopPants(
    workspace: Path,
    bloopDir: Path,
    target: String,
    json: Value
) {

  private val cycles = Cycles.findConnectedComponents(json)
  private val scalaCompiler = "org.scala-lang:scala-compiler:"
  private val nonAlphanumeric = "[^a-zA-Z0-9]".r

  private val transitiveClasspath = mutable.Map.empty[String, List[Path]]
  private val isVisited = mutable.Set.empty[String]

  val targets = json.obj("targets").obj
  val libraries = json.obj("libraries").obj
  val scalaJars = libraries.collect {
    case (module, jar) if isScalaJar(module) =>
      Paths.get(jar.obj("default").str)
  }.toSeq
  val compilerVersion = libraries.keysIterator
    .collectFirst {
      case module if module.startsWith(scalaCompiler) =>
        module.stripPrefix(scalaCompiler)
    }
    .getOrElse {
      throw new NoSuchElementException(scalaCompiler)
    }

  def run(): Int = {
    // Create Bloop projects that only contain the classpath of direct
    // dependencies but not transitive dependencies.
    val shallowClasspathProjects = targets.collect {
      case (id, target) if isSupportedTargetType(target) =>
        toBloopProject(id, target)
    }
    val byName = shallowClasspathProjects.map(p => p.name -> p).toMap

    // Add full transitive classpath to Bloop projects.
    val fullClasspathProjects = shallowClasspathProjects.map { p =>
      val children = cycles.children.getOrElse(p.name, Nil)
      val extraSources = children.flatMap(child => byName(child).sources)
      val extraDependencies = children
        .flatMap(child => byName(child).dependencies)
        .filter(_ != p.name)
      p.copy(
        classpath = getTransitiveClasspath(p.name, byName),
        sources = (p.sources ++ extraSources).distinct,
        dependencies = (p.dependencies ++ extraDependencies).distinct
      )
    }

    var generatedProjects = Set.empty[Path]
    fullClasspathProjects.foreach { bloop =>
      if (cycles.parents.contains(bloop.name)) {
        // Skip projects that are been eliminated due to cyclic dependencies.
      } else {
        val out = bloopDir.resolve(makeFilename(bloop.name) + ".json")
        val json = C.File(BuildInfo.bloopVersion, bloop)
        _root_.bloop.config.write(json, out)
        generatedProjects += out
      }
    }
    cleanStaleBloopFiles(generatedProjects)
    fullClasspathProjects.size
  }

  private val unsupportedTargetType = Set(
    "files", "page", "python_binary", "python_tests", "python_library",
    "python_requirement_library"
  )
  private def isSupportedTargetType(target: Value): Boolean =
    // Instead of whitelisting which target types we support, we hardcode which
    // known target types we know we don't support. Pants plugins can introduce
    // new target types that are minor variations of for example `scala_library`
    // that we may want to support.
    !unsupportedTargetType.contains(target.obj("pants_target_type").str)

  private def toBloopProject(
      id: String,
      target: Value
  ): C.Project = {

    val baseDirectories = for {
      roots <- target.obj.get("roots").toList
      root <- roots.arr
      sourceRoot <- root.obj.get("source_root")
    } yield Paths.get(sourceRoot.str)
    val baseDirectory = baseDirectories.headOption.getOrElse(workspace)

    val sources = Globs.fromTarget(workspace, target, baseDirectories).sources()

    // Extract target dependencies.
    val dependsOn = (for {
      dependency <- target.obj("targets").arr
      acyclicDependency = cycles.acyclicDependency(dependency.str)
      if acyclicDependency != id
      if isSupportedTargetType(targets(acyclicDependency))
    } yield acyclicDependency).toList

    // Extract class directories of direct target dependencies.
    val dependencyClassdirectories: List[Path] = (for {
      dependency <- target.obj("targets").arr
    } yield makeClassdirectory(dependency.str)).toList

    // Extract 3rd party library dependencies, and their associated sources.
    val libraryDependencies = for {
      dependency <- target.obj("libraries").arr
      library <- libraries.get(dependency.str)
    } yield library
    val libraryDependencyConfigs =
      List("default", "shaded", "linux-x86_64", "thrift9")
    val libraryDependencyClasspaths =
      getLibraryDependencies(libraryDependencies, libraryDependencyConfigs)
    val sourcesConfigs = List("sources")
    val libraryDependencySources =
      getLibraryDependencies(libraryDependencies, sourcesConfigs)
        .map(newSourceModule)

    // Warn about unfamiliar configurations.
    val knownConfigs = sourcesConfigs ::: libraryDependencyConfigs
    val unknownConfigs = libraryDependencies.flatMap(
      lib => lib.obj.keys.toSet -- knownConfigs
    )
    if (unknownConfigs.nonEmpty) {
      println(
        s"[warning] Unknown configs: ${unknownConfigs.mkString(",")}"
      )
    }

    val out = bloopDir.resolve(makeFilename(id))
    val classDirectory = Files.createDirectories(out.resolve("classes"))
    val javaHome = Option(System.getProperty("java.home")).map(Paths.get(_))

    C.Project(
      id,
      directory = baseDirectory,
      sources,
      dependencies = dependsOn,
      dependencyClassdirectories ++ libraryDependencyClasspaths,
      out,
      classDirectory,
      scala = Some(
        C.Scala(
          "org.scala-lang",
          "scala-compiler",
          compilerVersion,
          List.empty[String],
          scalaJars.toList,
          None,
          setup = Some(
            C.CompileSetup(
              C.Mixed,
              addLibraryToBootClasspath = true,
              addCompilerToClasspath = false,
              addExtraJarsToClasspath = false,
              manageBootClasspath = true,
              filterLibraryFromClasspath = true
            )
          )
        )
      ),
      java = Some(C.Java(Nil)),
      sbt = None,
      test = None,
      platform = Some(C.Platform.Jvm(C.JvmConfig(javaHome, Nil), None)),
      resolution = Some(C.Resolution(libraryDependencySources.toList)),
      resources = None
    )
  }

  private def makeClassdirectory(target: String): Path =
    bloopDir.resolve(makeFilename(target)).resolve("classes")

  private def getLibraryDependencies(
      libraryDependencies: Seq[Value],
      keys: List[String]
  ): Seq[Path] =
    for {
      lib <- libraryDependencies
      path <- keys.collectFirst {
        case key if lib.obj.contains(key) => lib.obj(key)
      }
    } yield Paths.get(path.str)

  private def newSourceModule(source: Path) =
    C.Module(
      "",
      "",
      "",
      None,
      artifacts = List(
        C.Artifact(
          "",
          classifier = Some("sources"),
          None,
          path = source
        )
      )
    )

  private def getTransitiveClasspath(
      name: String,
      byName: Map[String, C.Project]
  ): List[Path] = {
    def computeTransitiveClasspath(): List[Path] = {
      val buf = mutable.Set.empty[Path]
      buf ++= byName(name).classpath
      byName(name).dependencies.foreach { dep =>
        buf ++= getTransitiveClasspath(dep, byName)
      }
      val children = cycles.children.getOrElse(name, Nil)
      children.foreach { child =>
        buf ++= getTransitiveClasspath(child, byName)
      }

      // NOTE: Pants automatically includes the compiler classpath for all
      // targets causing some targets to have an undeclared dependency on
      // scala-compiler even if they don't compile without scala-compiler on the
      // classpath.
      buf ++= scalaJars

      buf.toList.sorted
    }
    if (isVisited(name)) {
      transitiveClasspath.getOrElse(name, Nil)
    } else {
      isVisited += name
      transitiveClasspath.getOrElseUpdate(name, computeTransitiveClasspath())
    }
  }

  private def isScalaJar(module: String): Boolean =
    module.startsWith(scalaCompiler) ||
      module.startsWith("org.scala-lang:scala-reflect:") ||
      module.startsWith("org.scala-lang:scala-library:") ||
      module.startsWith("org.scala-lang:scala-library:") ||
      module.startsWith("org.fursesource:jansi:") ||
      module.startsWith("jline:jline:")

  private def cleanStaleBloopFiles(
      generatedProjects: Set[Path]
  ): Unit = {
    val ls = Files.list(bloopDir)
    try {
      ls.filter { path =>
          Files.isRegularFile(path) &&
          path.getFileName().toString().endsWith(".json") &&
          !generatedProjects(path)
        }
        .forEach { path =>
          Files.delete(path)
        }
    } finally {
      ls.close()
    }
  }

  private def makeFilename(target: String): String = {
    nonAlphanumeric.replaceAllIn(target, "")
  }

}
