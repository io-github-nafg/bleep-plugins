package bleep.plugins.scoverage

import bleep.BuildPaths
import bleep.model.*
import bleep.rewrites.BuildRewrite

object ScoverageRewrite extends BuildRewrite {
  private val scoverageVersion = "2.3.0"

  override val name = BuildRewriteName("scoverage")

  override protected def newExplodedProjects(oldBuild: Build, buildPaths: BuildPaths) =
    oldBuild.explodedProjects.transform { (crossProjectName, project) =>
      if (project.isTestProject.contains(true))
        project
      else
        project.copy(
          scala = project.scala.map { s =>
            val dataDir = coverageDataDir(buildPaths.project(crossProjectName, project))
            s.copy(
              options = s.options
                .union(
                  Options(
                    if (s.version.exists(_.is3))
                      Set(
                        Options.Opt.Flag(s"-coverage-out:$dataDir"),
                        // noinspection SpellCheckingInspection
                        Options.Opt.Flag(s"-sourceroot:${buildPaths.buildDir}")
                      )
                    else
                      Set(
                        Options.Opt.Flag(s"-P:scoverage:dataDir:$dataDir"),
                        Options.Opt.Flag(s"-P:scoverage:sourceRoot:${buildPaths.buildDir}")
                      )
                  )
                ),
              compilerPlugins =
                if (s.version.exists(_.is3))
                  s.compilerPlugins
                else
                  s.compilerPlugins +
                    Dep.ScalaFullVersion("org.scoverage", "scalac-scoverage-plugin", scoverageVersion)
            )
          },
          dependencies =
            if (project.scala.exists(_.version.exists(_.is3)))
              project.dependencies
            else
              project.dependencies +
                Dep.Scala("org.scoverage", "scalac-scoverage-runtime", scoverageVersion)
        )
    }
}
