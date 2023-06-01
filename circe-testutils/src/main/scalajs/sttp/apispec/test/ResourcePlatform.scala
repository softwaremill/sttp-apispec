package sttp.apispec.test

import io.circe.{Json, Error}
import io.circe.parser.decode

trait ResourcePlatform {

  /** @return
    *   Base directory of sbt project we should read resources from
    */
  def basedir: String
  def resourcesPath(path: String): String = s"$basedir/src/test/resources$path"

  def resourceAsString(path: String): String = {
    import scalajs.js.Dynamic.{global => g}
    val fs = g.require("fs")

    def readFile(name: String): String = {
      fs.readFileSync(name).toString
    }

    readFile(resourcesPath(path))
  }

  def readJson(path: String): Either[Error, Json] = {
    val string = resourceAsString(path)
    decode[Json](string)
  }
}
