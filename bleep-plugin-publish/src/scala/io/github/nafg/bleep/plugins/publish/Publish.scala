package io.github.nafg.bleep.plugins.publish

import java.nio.file.Files

import scala.collection.immutable.SortedMap

import bleep.*
import bleep.model.CrossProjectName
import bleep.nosbt.InteractionService
import bleep.packaging.{CoordinatesFor, PackagedLibrary, PublishLayout, packageLibraries}
import bleep.plugin.cirelease.CiReleasePlugin
import bleep.plugin.dynver.DynVerPlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype
import coursier.Info
import ryddig.Formatter

object Publish extends BleepScript("Publish") {
  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val config =
      yaml.decode[PublishConfig](Files.readString(started.buildPaths.buildDir / "bleep.publish.yaml")).toTry.get

    val projectsToPublishName = config.projects.toSet
    val projectsToPublish     = projectsToPublishName.compose[CrossProjectName](_.name.value)

    commands.compile(started.build.explodedProjects.keys.filter(projectsToPublish).toList)

    val dynVer    = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val pgp       = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )
    val sonatype  = new Sonatype(
      logger = started.logger,
      sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
      sonatypeProfileName = config.sonatype.profileName,
      bundleName = config.sonatype.bundleName,
      version = dynVer.version,
      sonatypeCredentialHost = Sonatype.sonatypeLegacy
    )
    val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

    started.logger.info(dynVer.version)

    val info = Info(
      description = config.info.description,
      homePage = config.info.homePage,
      developers = config.info.developers.map { case PublishConfig.Info.Developer(id, name, url) =>
        Info.Developer(id, name, url)
      },
      publication = None,
      scm = CiReleasePlugin.inferScmInfo,
      licenseInfo = config.info.licenseInfo.map { case PublishConfig.Info.License(name, url, distribution, comments) =>
        Info.License(name, url, distribution, comments)
      }
    )

    val packagedLibraries: SortedMap[CrossProjectName, PackagedLibrary] =
      packageLibraries(
        started,
        coordinatesFor = CoordinatesFor.Default(groupId = config.groupId, version = dynVer.version),
        shouldInclude = projectsToPublish,
        publishLayout = PublishLayout.Maven(info)
      )

    val files: Map[RelPath, Array[Byte]] =
      packagedLibraries.flatMap { case (_, PackagedLibrary(_, files)) => files.all }

    files.foreach { case (path, bytes) =>
      implicit def relPathFormatter: Formatter[RelPath] = _.asString
      started.logger.withContext("path", path).withContext("bytes.length", bytes.length).debug("will publish")
    }

    ciRelease.ciRelease(files)
  }
}
