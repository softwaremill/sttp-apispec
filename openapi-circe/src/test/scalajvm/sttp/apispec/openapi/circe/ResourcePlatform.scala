package sttp.apispec.openapi.circe

import io.circe._
import io.circe.parser.decode
import java.io.{BufferedReader, InputStreamReader, StringWriter}
import java.nio.charset.StandardCharsets

trait ResourcePlatform {
  def readJson(path: String): Either[Error, Json] = {
    val is = getClass.getResourceAsStream(path)
    val reader = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
    val writer = new StringWriter()
    reader.transferTo(writer)
    decode[Json](writer.toString)
  }
}