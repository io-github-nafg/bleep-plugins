package bleep.plugins.scoverage

import java.nio.file.{Files, Path}

import scala.io.Codec

import bleep.rewrites.BuildRewrite
import bleep.{BleepScript, Commands, PathOps, Started}
import caseapp.core.app.{Command, CommandsEntryPoint}
import caseapp.core.argparser.{ArgParser, SimpleArgParser}
import caseapp.core.parser.Parser
import caseapp.core.{Error, RemainingArgs}
import caseapp.{ArgsName, HelpMessage, ValueDescription}
import cats.data.NonEmptyList
import scoverage.domain.Coverage
import scoverage.reporter.{CoberturaXmlWriter, CoverageAggregator, ScoverageHtmlWriter, ScoverageXmlWriter}

object ScoverageScript extends BleepScript("scoverage") {
  override val rewrites: List[BuildRewrite] = List(ScoverageRewrite)

  sealed trait Reporter {
    def write(coverage: Coverage, sourceDirs: Seq[Path], reportDir: Path): Unit
  }
  object Reporter       {
    case object Html      extends Reporter {
      override def write(coverage: Coverage, sourceDirs: Seq[Path], reportDir: Path): Unit = {
        Files.createDirectories(reportDir)
        new ScoverageHtmlWriter(sourceDirs.map(_.toFile), reportDir.toFile).write(coverage)
      }
    }
    case object Xml       extends Reporter {
      override def write(coverage: Coverage, sourceDirs: Seq[Path], reportDir: Path): Unit = {
        Files.createDirectories(reportDir)
        new ScoverageXmlWriter(sourceDirs.map(_.toFile), reportDir.toFile, false, Some(Codec.UTF8.name)).write(coverage)
      }
    }
    case object Cobertura extends Reporter {
      override def write(coverage: Coverage, sourceDirs: Seq[Path], reportDir: Path): Unit = {
        Files.createDirectories(reportDir)
        new CoberturaXmlWriter(sourceDirs.map(_.toFile), reportDir.toFile, Some(Codec.UTF8.name)).write(coverage)
      }
    }
    case object Console   extends Reporter {
      override def write(coverage: Coverage, sourceDirs: Seq[Path], reportDir: Path): Unit = {
        println(s"Statement coverage.: ${coverage.statementCoverageFormatted}%")
        println(s"Branch coverage....: ${coverage.branchCoverageFormatted}%")
      }
    }

    implicit val reporterParser: ArgParser[Reporter] = SimpleArgParser.from("reporter") {
      case "html"      => Right(Html)
      case "xml"       => Right(Xml)
      case "cobertura" => Right(Cobertura)
      case "console"   => Right(Console)
      case _           => Left(Error.MalformedValue("reporter", "should be one of html, xml, cobertura, or console"))
    }
  }

  @ArgsName("test-project...")
  case class TestOptions(only: List[String], exclude: List[String])
  object TestOptions   {
    implicit val parser: Parser[TestOptions] = Parser.derive
  }
  case class ReportOptions(
    @HelpMessage("The reporter to use. Can be passed multiple times. Defaults to all.")
    @ValueDescription("html|xml|cobertura|console*") reporter: List[Reporter] =
      List(Reporter.Html, Reporter.Xml, Reporter.Cobertura, Reporter.Console)
  )
  object ReportOptions {
    implicit val parser: Parser[ReportOptions] = Parser.derive
  }

  class CoverageApp(started: Started, bleepCommands: Commands) extends CommandsEntryPoint {
    override def progName = "bleep coverage --"

    object testCommand extends Command[TestOptions] {
      override def run(options: TestOptions, remainingArgs: RemainingArgs): Unit = {
        val selectedTestProjectNames =
          remainingArgs.all.flatMap { str =>
            val projectNames = started.globs.testProjectNameMap(str)
            started.logger.info(s"Running tests in ${projectNames.map(_.value).mkString(", ")}")
            projectNames
          }
        bleepCommands.test(
          projects = started.chosenTestProjects(Some(selectedTestProjectNames.toArray)).toList,
          watch = false,
          testOnlyClasses = NonEmptyList.fromList(options.only),
          testExcludeClasses = NonEmptyList.fromList(options.exclude)
        )
      }
    }

    object reportCommand extends Command[ReportOptions] {
      private lazy val (coverageProjects, coverageDataDirs) =
        started.build.explodedProjects
          .filterNot(_._2.isTestProject.contains(true))
          .keySet
          .map(name => name -> coverageDataDir(started.projectPaths(name)).toFile)
          .filter(_._2.exists())
          .toSeq
          .unzip

      override def run(options: ReportOptions, remainingArgs: RemainingArgs): Unit = {
        started.logger.info(s"Aggregating coverage data from ${coverageProjects.map(_.value).mkString(", ")}")
        CoverageAggregator.aggregate(coverageDataDirs, started.buildPaths.buildDir.toFile) match {
          case None           => started.logger.error("No coverage data found")
          case Some(coverage) =>
            val outDir = started.buildPaths.dotBleepDir / "coverage-report"
            started.logger.info(s"Coverage report directory is $outDir")
            options.reporter.foreach { reporter =>
              started.logger.info(s"Writing coverage report using ${reporter.toString}")
              reporter.write(coverage, coverageProjects.flatMap(started.projectPaths(_).sourcesDirs.all), outDir)
            }
        }
      }
    }

    override def commands = Seq[Command[?]](testCommand, reportCommand)
  }

  def run(started: Started, bleepCommands: Commands, args: List[String]): Unit =
    new CoverageApp(started, bleepCommands).main(args.toArray)
}
