package io.github.nafg.bleep.plugins.publish

import java.io.FileOutputStream
import java.nio.file.Files
import java.time.{Duration, Instant}
import java.util.Base64
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import bleep.*
import bleep.commands.PublishLocal
import bleep.model.CrossProjectName
import bleep.nosbt.InteractionService
import bleep.packaging.*
import bleep.plugin.cirelease.CiReleasePlugin
import bleep.plugin.dynver.DynVerPlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype
import coursier.Info
import ryddig.{Formatter, processLogger}
import sttp.client4.*
import sttp.model.Header

object Publish extends BleepScript("Publish") {
  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    import started.logger

    val config =
      yaml.decode[PublishConfig](Files.readString(started.buildPaths.buildDir / "bleep.publish.yaml")).toTry.get

    val projectsToPublishName = config.projects.toSet
    val isProjectToPublish    = projectsToPublishName.compose[CrossProjectName](_.name.value)
    val projectsToPublish     = started.build.explodedProjects.keys.filter(isProjectToPublish)

    commands.compile(projectsToPublish.toList)

    val dynVer = new DynVerPlugin(
      baseDirectory = started.buildPaths.buildDir.toFile,
      dynverSeparator = "-",
      dynverSonatypeSnapshots = false
    )

    val pgp = new PgpPlugin(
      logger = logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )

    val version = dynVer.version

    logger.info(s"Publishing version $version")

    val info = Info(
      description = config.info.description,
      homePage = config.info.homePage,
      developers = config.info.developers.map { case PublishConfig.Info.Developer(id, name, url) =>
        Info.Developer(id, name, url)
      },
      publication = None,
      scm = config.info.scm
        .map { case PublishConfig.Info.Scm(url, connection, developerConnection) =>
          Info.Scm(url, connection, developerConnection)
        }
        .orElse(CiReleasePlugin.inferScmInfo),
      licenseInfo = config.info.licenseInfo.map { case PublishConfig.Info.License(name, url, distribution, comments) =>
        Info.License(name, url, distribution, comments)
      }
    )

    val packagedLibraries: SortedMap[CrossProjectName, PackagedLibrary] =
      packageLibraries(
        started,
        coordinatesFor = CoordinatesFor.Default(groupId = config.groupId, version = version),
        shouldInclude = isProjectToPublish,
        publishLayout = PublishLayout.Maven(info)
      )

    val files: Map[RelPath, Array[Byte]] =
      packagedLibraries.flatMap { case (_, PackagedLibrary(_, files)) => files.all }

    files.foreach { case (path, bytes) =>
      implicit def relPathFormatter: Formatter[RelPath] = _.asString
      logger.withContext("path", path).withContext("bytes.length", bytes.length).debug("will publish")
    }

    val profileName = config.sonatype.profileName
    val bundleName  = config.sonatype.bundleName

    def ciReleasePlugin(host: String) =
      new CiReleasePlugin(
        logger = logger,
        sonatype = new Sonatype(
          logger = logger,
          sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
          sonatypeProfileName = profileName,
          bundleName = bundleName,
          version = version,
          sonatypeCredentialHost = host
        ),
        dynVer = dynVer,
        pgp = pgp
      )

    def sendSonatypeRequest(request: Request[Either[String, String]]) = {
      val sonatypeToken =
        Base64.getEncoder.encodeToString(s"${sys.env("SONATYPE_USERNAME")}:${sys.env("SONATYPE_PASSWORD")}".getBytes)
      request
        .header(Header.authorization("Bearer", credentials = s"$sonatypeToken"))
        .send(DefaultSyncBackend())
        .body match {
        case Right(value) => value
        case Left(value)  =>
          logger.error(value)
          sys.exit(1)
      }
    }

    args match {
      case List("--mode=local")                       =>
        commands.publishLocal(
          PublishLocal.Options(
            groupId = profileName,
            version = version,
            publishTarget = PublishLocal.LocalIvy,
            projects = projectsToPublish.toArray,
            manifestCreator = ManifestCreator.default
          )
        )
      case List() | List("--mode=sonatype-legacy")    =>
        ciReleasePlugin(Sonatype.sonatypeLegacy).ciRelease(files)
      case List("--mode=ossrh-staging")               =>
        val baseUrl = "https://ossrh-staging-api.central.sonatype.com"

        ciReleasePlugin(baseUrl).ciRelease(files)

        sendSonatypeRequest(
          basicRequest
            .post(
              uri"$baseUrl/manual/upload/defaultRepository/$profileName".addParams("publishing_type" -> "automatic")
            )
        ).discard()
      case List(s"--mode=portal-api:$publishingType") =>
        CiReleasePlugin.setupGpg(processLogger(logger, "setupGpg"))

        logger.warn(s"signing ${files.size} files")
        val signed   = pgp.signedArtifacts(files)
        logger.warn(s"digesting ${signed.size} files")
        val digested = Checksums(signed, List(Checksums.Algorithm.Md5, Checksums.Algorithm.Sha1))

        val bundleFile = started.buildPaths.dotBleepDir / "portal-bundle" / s"$bundleName.zip"
        Files.createDirectories(bundleFile.getParent)

        val zipOut = new ZipOutputStream(new FileOutputStream(bundleFile.toFile))
        digested.foreach { case (relPath, bytes) =>
          zipOut.putNextEntry(new ZipEntry(relPath.asString))
          zipOut.write(bytes)
          zipOut.closeEntry()
        }
        zipOut.close()

        logger.info(s"Created bundle with ${digested.size} files")

        logger.info("📦 Uploading bundle to Portal API...")
        val deploymentId = sendSonatypeRequest(
          basicRequest
            .post(
              uri"https://central.sonatype.com/api/v1/publisher/upload"
                .addParams("name" -> s"$profileName.$bundleName-$version", "publishingType" -> publishingType)
            )
            .multipartBody(multipartFile("bundle", bundleFile.toFile))
        )
        logger.info(s"✅ Upload complete! Deployment ID: $deploymentId")

        logger.info("⏳ Waiting for validation...")

        val startTime = Instant.now()
        val interval  = if (System.console() == null) 30_000 else 10_000

        @tailrec
        def pollUntilComplete(): Unit = {
          val json = ujson.read(
            sendSonatypeRequest(
              basicRequest
                .post(uri"https://central.sonatype.com/api/v1/publisher/status?id=$deploymentId")
                .header("Content-Type", "application/json")
            )
          )

          def logPurls() = {
            val purls = json("purls").arr.map(_.str)
            logger.info(s"📊 ${purls.length} components:")
            purls.foreach(str => logger.info(s"  • $str"))
          }

          json("deploymentState").str match {
            case "VALIDATED" =>
              logger.info("✅ Deployment validated successfully!")
              logPurls()
              logger.info("💡 Ready for manual publish via Portal UI")
            case "PUBLISHED" =>
              logger.info("🚀 Deployment published successfully!")
              logPurls()
            case "FAILED"    =>
              logger.error("❌ Deployment validation failed!")
              json("errors").obj.foreach { case (component, errorList) =>
                logger.error(s"  ❌ $component:")
                errorList.arr.foreach(error => logger.error(s"     • ${error.str}"))
              }
              sys.exit(1)
            case state       =>
              val elapsed = Duration.between(startTime, Instant.now())
              logger.info(s"🔄 Status: $state (${elapsed.toMinutes}m${elapsed.toSecondsPart}s)")

              Thread.sleep(interval)
              pollUntilComplete()
          }
        }

        pollUntilComplete()

      case _ =>
        logger.error("""Invalid arguments.
            |Usage: bleep publish -- [--mode=[local|sonatype-legacy|ossrh-staging|portal-api:[USER_MANAGED|AUTOMATIC]]
            |""".stripMargin)
        System.exit(2)
    }
  }
}
