package sttp.apispec
package openapi
package circe

import org.scalatest.funsuite.AnyFunSuite

class DecoderTest extends AnyFunSuite with ResourcePlatform {
  test("petstore deserialize") {
    val Right(openapi) = readJson("/petstore/basic-petstore.json").flatMap(_.as[OpenAPI])

    assert(openapi.info.description === Some("This is a sample server for a pet store."))
  }

  test("spec any nothing schema") {
    val Right(openapi) = readJson("/spec/3.1/any_and_nothing.json").flatMap(_.as[OpenAPI])

    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.nonEmpty)
    assert(schemas("anything_boolean") === Right(AnySchema.Anything(AnySchema.Encoding.Boolean)))
    assert(schemas("nothing_boolean") === Right(AnySchema.Nothing(AnySchema.Encoding.Boolean)))
    assert(schemas("anything_object") === Right(AnySchema.Anything(AnySchema.Encoding.Object)))
    assert(schemas("nothing_object") === Right(AnySchema.Nothing(AnySchema.Encoding.Object)))
  }
}
