package sttp.apispec.test

import io.circe._
import io.circe.parser.decode
import java.io.{BufferedReader, InputStreamReader, StringWriter}
import java.nio.charset.StandardCharsets

trait ResourcePlatform {

  /**
   * @return Base directory of sbt project we should read resources from. Not used from JVM
   */
  def basedir: String
  def readJson(path: String): Either[Error, Json] = {

    val is = getClass.getResourceAsStream(path)
    val reader = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
    val writer = new StringWriter()
    reader.transferTo(writer)
    decode[Json](writer.toString)
  }
}
