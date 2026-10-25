package sttp.apispec

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.circe._
import sttp.apispec.test._
import io.circe.syntax._
import io.circe.Decoder
import io.circe.parser.parse

class RoundTripTest extends AnyFunSuite with ResourcePlatform {
  override val basedir = "jsonschema-circe"

  test("Can parse self-encoded schema") {
    val simple = Schema(
      $schema = Some("https://json-schema.org/draft/2020-12/schema"),
      $id = Some("http://yourdomain.com/schemas/myschema.json")
    )
    val decoded = Decoder[Schema].decodeJson(simple.asJson)
    assert(decoded.isRight, decoded.fold(_.toString, _ => ""))
  }

  test("decoding and encoding keeps example values") {
    val Right(json) = parse(
      """{"examples":[200,9007199254740993,1.00000000000000000001,true,{"a":1},[1,{"b":2}],["a",1,true,null]]}"""
    ): @unchecked
    assert(Decoder[Schema].decodeJson(json).map(_.asJson) == Right(json))
  }
}
