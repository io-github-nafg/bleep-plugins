package io.github.nafg.bleep.plugins.projectsgraph

import java.nio.file.{Files, Paths}

import bleep.model.CrossProjectName
import bleep.{BleepScript, Commands, DiscardOps, Started}

object ProjectsGraph extends BleepScript("moduleDepGraph") {
  override def run(started: Started, commands: Commands, args: List[String]): Unit = args match {
    case List(filename) =>
      val path = Paths.get(filename)

      Files.createDirectories(path.getParent)

      val nameToNames =
        started.build.explodedProjects.map { case (CrossProjectName(projectName, _), project) =>
          projectName.value ->
            project.dependsOn.values.map(_.value)
        }

      val lines =
        nameToNames.toSeq.sortBy(_._1).map { case (from, tos) =>
          (s"    $from" +:
            tos.toSeq.map(to => s"    $from-->$to")).mkString("\n")
        }

      Files
        .writeString(
          path,
          // language=md
          s"""# Projects dependency graph
          |
          |```mermaid
          |graph TD
          |${lines.mkString("\n")}
          |```
          |""".stripMargin
        )
        .discard()

    case _ =>
      Console.err.println("usage: moduleDepGraph <output-file>")
      sys.exit(1)
  }
}
