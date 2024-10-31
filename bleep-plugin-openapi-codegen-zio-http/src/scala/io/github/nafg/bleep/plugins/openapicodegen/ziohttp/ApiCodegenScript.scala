package io.github.nafg.bleep.plugins.openapicodegen.ziohttp

import java.nio.file.Files

import scala.io.Source
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Using

import bleep.{BleepCodegenScript, Commands, PathOps, Started}
import zio.http.endpoint.openapi.OpenAPI
import zio.http.gen.openapi.EndpointGen
import zio.http.gen.scala.{Code, CodeGen}
import zio.json.yaml.*
import zio.schema.codec.JsonCodec

object ApiCodegenScript extends BleepCodegenScript("api-codegen") {
  override def run(started: Started, commands: Commands, targets: List[ApiCodegenScript.Target], args: List[String]) = {
    targets.foreach { target =>
      val dir = started.projectPaths(target.project).dir / "openapi"
      if (Files.exists(dir) && Files.isDirectory(dir)) {
        val yamlFiles =
          Files
            .walk(dir)
            .iterator()
            .asScala
            .filter(path => path.toString.endsWith(".yaml") || path.toString.endsWith(".yml"))

        for (yamlFile <- yamlFiles)
          Using.resource(Source.fromFile(yamlFile.toFile)) { source =>
            val relative = dir.relativize(yamlFile)
            val dirs     = relative.getParent.iterator().asScala.map(_.toString).toList
            source.mkString.fromYaml(JsonCodec.jsonDecoder(OpenAPI.schema)) match {
              case Left(value)    => sys.error(value)
              case Right(openAPI) =>
                val files0 = EndpointGen.fromOpenAPI(openAPI)
                val files  =
                  files0.copy(files = files0.files.map { f =>
                    f.copy(imports = (f.imports :+ Code.Import("zio.schema._")).distinct.map {
                      case Code.Import.Absolute(path) => Code.Import.Absolute(path.replaceFirst("\\._$", ".*"))
                      case Code.Import.FromBase(path) => Code.Import.FromBase(path.replaceFirst("\\._$", ".*"))
                    })
                  })
                CodeGen.writeFiles(
                  files = files,
                  basePath = target.sources.resolve(relative) / "generated",
                  basePackage = s"${dirs.mkString(".")}.generated",
                  scalafmtPath = Some(started.buildPaths.buildDir / ".scalafmt.conf")
                )
            }
          }
      } else
        started.logger.warn(s"$dir does not exist or is not a directory")
    }
  }
}
