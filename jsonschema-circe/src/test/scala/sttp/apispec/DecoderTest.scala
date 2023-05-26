package sttp.apispec

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.circe._
import sttp.apispec.test._

import scala.collection.immutable.ListMap

class DecoderTest extends AnyFunSuite with ResourcePlatform {
  val basedir = "jsonschema-circe"

  test("extending rescursive") {
    val Right(json) = readJson("/extending-recursive.json")
    val schema = json.as[Schema]
    assert(schema.isRight)
    val unsafeSchema = schema.right.get
    val adrEither = unsafeSchema.$defs.getOrElse(ListMap.empty)("address")

    adrEither match {
      case s: Schema => assert(s.$schema == Some("http://json-schema.org/draft-07/schema#") && s.$defs.isDefined)
      case _         => fail("Nope")
    }
  }

  test("self-decribing-schema") {
    val Right(json) = readJson("/self-describing-schema.json")
    val schema = json.as[Schema]
    assert(schema.isRight)

    val unsafeSchema = schema.right.get
    assert(unsafeSchema.description === Some("Meta-schema for self-describing JSON schema"))

  }
}
