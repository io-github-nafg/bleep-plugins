package io.github.nafg.bleep.plugins.publish

import scala.annotation.unused

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

case class PublishConfig(
  groupId: String,
  projects: List[String],
  sonatype: PublishConfig.Sonatype,
  info: PublishConfig.Info
)
object PublishConfig {
  case class Sonatype(profileName: String, bundleName: String)

  case class Info(
    description: String,
    homePage: String,
    developers: List[Info.Developer],
    licenseInfo: List[Info.License]
  )
  object Info {
    case class Developer(id: String, name: String, url: String)
    case class License(name: String, url: Option[String], distribution: Option[String], comments: Option[String])

    @unused
    private implicit val decodeDeveloper: Decoder[Developer] = deriveDecoder[Developer]
    @unused
    private implicit val decodeLicense: Decoder[License]     = deriveDecoder[License]

    implicit val decodeInfo: Decoder[Info] = deriveDecoder[Info]
  }

  @unused
  private implicit val decodeSonatype: Decoder[Sonatype]   = deriveDecoder[Sonatype]
  implicit val decodePublishConfig: Decoder[PublishConfig] = deriveDecoder[PublishConfig]
}
