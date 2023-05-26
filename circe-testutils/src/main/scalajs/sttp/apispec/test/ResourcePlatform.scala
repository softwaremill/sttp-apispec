package sttp.apispec.test

import io.circe.{Json, Error}
import io.circe.parser.decode

trait ResourcePlatform {
  def basedir: String
  def rscPath(path: String): String = s"$basedir/src/test/resources$path"

  def rsc(path: String): String = {
    import scalajs.js.Dynamic.{global => g}
    val fs = g.require("fs")

    def readFile(name: String): String = {
      fs.readFileSync(name).toString
    }

    readFile(rscPath(path))
  }

  def readJson(path: String): Either[Error, Json] = {
    val string = rsc(path)
    decode[Json](string)
  }
}
