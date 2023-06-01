package sttp.apispec.test

import io.circe._
import io.circe.parser.decode

trait ResourcePlatform {

  /** @return
    *   Base directory of sbt project we should read resources from
    */
  def basedir: String
  def resourcesPath(path: String): String = s"$basedir/src/test/resources/$path"

  def readJson(path: String): Either[Error, Json] = {
    val string = scala.io.Source.fromFile(resourcesPath(path), "UTF-8").mkString
    decode[Json](string)
  }
}
