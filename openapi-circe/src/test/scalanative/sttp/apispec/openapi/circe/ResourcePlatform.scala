package sttp.apispec.openapi.circe

import io.circe._
import io.circe.parser.decode

trait ResourcePlatform {
  def rscPath(path: String): String = "openapi-circe/src/test/resources" + path

  def readJson(path: String): Either[Error, Json] = {
    val string = scala.io.Source.fromFile(rscPath(path), "UTF-8").mkString
    decode[Json](string)
  }
}
