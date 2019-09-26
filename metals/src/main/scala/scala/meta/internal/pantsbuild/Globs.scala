package scala.meta.internal.pantsbuild

import ujson._
import java.nio.file._
import java.util.stream.{Stream => JStream}
import java.util.stream.Collectors
import scala.collection.JavaConverters._

/**
 * An incomplete, buggy re-implementation of Pants glob expansion.
 *
 * The reason we re-implement the logic here instead of calling out to Pants is
 * to unblock basic usage of Metals with Pants. In the future, we should be
 * able to call out to Pants to perform the glob expansion for us.
 */
object Globs {
  def fromTarget(
      workspace: Path,
      target: Value,
      baseDirectories: Seq[Path]
  ): Globs = {
    target.obj.get("globs") match {
      case None =>
        val globs = baseDirectories.map { dir =>
          val path = workspace.resolve(dir).resolve("**")
          Str(path.toString())
        }
        Globs(workspace, globs, Nil)
      case Some(globs) =>
        val ex = globs.obj.get("exclude") match {
          case None => Nil
          case Some(excludes) =>
            for {
              exclude <- excludes.arr.toSeq
              globs <- exclude.obj.get("globs").toSeq
              glob <- globs.arr
            } yield mkGlob(workspace, glob.str)
        }
        Globs(workspace, globs.obj("globs").arr, ex)
    }
  }

  private def mkGlob(workspace: Path, str: String): PathMatcher = {
    FileSystems
      .getDefault()
      .getPathMatcher(
        "glob:" + workspace
          .resolve(str.replaceAllLiterally("**/*", "**"))
          .toString()
      )
  }
}

case class Globs(
    workspace: Path,
    globs: Seq[Value],
    excludes: Seq[PathMatcher]
) {

  def sources(): List[Path] = globs.iterator.flatMap(expandGlobToSources).toList
  private def expandGlobToSources(glob: Value): List[Path] = {
    if (glob.str.contains("*")) {
      val parent = workspace.resolve(glob.str).getParent()
      val includes = Globs.mkGlob(workspace, glob.str)
      val stream: JStream[Path] =
        if (parent.endsWith("**")) {
          if (Files.isDirectory(parent.getParent()))
            Files.walk(parent.getParent())
          else JStream.empty()
        } else if (Files.isDirectory(parent)) {
          Files.list(parent)
        } else {
          JStream.empty()
        }

      try {
        stream
          .map[Path] { path =>
            if (path.isAbsolute()) path.toAbsolutePath()
            else workspace.resolve(path).toAbsolutePath()
          }
          .filter { path =>
            Files.isRegularFile(path)
          }
          .filter { path =>
            includes.matches(path) &&
            !excludes.exists(_.matches(path))
          }
          .collect(Collectors.toList())
          .asScala
          .toList
      } finally {
        stream.close()
      }
    } else {
      val path = Paths.get(glob.str)
      val abspath =
        if (path.isAbsolute()) path
        else workspace.resolve(path)
      List(abspath)
    }
  }

}
