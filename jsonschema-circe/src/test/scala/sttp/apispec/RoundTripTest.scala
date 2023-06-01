package sttp.apispec

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.circe._
import sttp.apispec.test._
import io.circe.syntax._
import io.circe.Decoder

class RoundTripTest extends AnyFunSuite with ResourcePlatform {
  override val basedir = "jsonschema-circe"

  test("Can parse self-encoded schema") {
    val simple = Schema(
      $schema = Some("https://json-schema.org/draft/2020-12/schema"),
      $id = Some("http://yourdomain.com/schemas/myschema.json")
    )
    val decoded = Decoder[Schema].decodeJson(simple.asJson)
    assert(decoded.isRight)
  }
}
