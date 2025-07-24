package io.github.nafg.bleep.plugins.publish

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
    licenseInfo: List[Info.License],
    scm: Option[Info.Scm]
  )
  object Info {
    case class Developer(id: String, name: String, url: String)
    case class License(name: String, url: Option[String], distribution: Option[String], comments: Option[String])
    case class Scm(url: Option[String], connection: Option[String], developerConnection: Option[String])

    private implicit val decodeDeveloper: Decoder[Developer] = deriveDecoder[Developer]
    private implicit val decodeLicense: Decoder[License]     = deriveDecoder[License]
    private implicit val decodeScm: Decoder[Scm]             = deriveDecoder[Scm]

    implicit val decodeInfo: Decoder[Info] = deriveDecoder[Info]
  }

  private implicit val decodeSonatype: Decoder[Sonatype]   = deriveDecoder[Sonatype]
  implicit val decodePublishConfig: Decoder[PublishConfig] = deriveDecoder[PublishConfig]
}
