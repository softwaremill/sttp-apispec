package sttp.apispec.test

import io.circe._
import io.circe.parser.decode
import scala.io.Source

trait ResourcePlatform {

  /** @return
    *   Base directory of sbt project we should read resources from. Not used from JVM
    */
  def basedir: String
  def readJson(path: String): Either[Error, Json] = {
    val is = getClass.getResourceAsStream(path)
    try decode[Json](Source.fromInputStream(is, "UTF-8").mkString)
    finally is.close()
  }
}
