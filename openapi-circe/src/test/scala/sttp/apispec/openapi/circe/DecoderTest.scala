package sttp.apispec
package openapi
package circe

import sttp.apispec.test._
import org.scalatest.funsuite.AnyFunSuite

class DecoderTest extends AnyFunSuite with ResourcePlatform {
  override val basedir = "openapi-circe"

  test("petstore deserialize") {
    val Right(openapi) = readJson("/petstore/basic-petstore.json").flatMap(_.as[OpenAPI]): @unchecked

    assert(openapi.info.description === Some("This is a sample server for a pet store."))
  }

  test("spec any nothing schema boolean") {
    val Right(openapi) = readJson("/spec/3.1/any_and_nothing1.json").flatMap(_.as[OpenAPI]): @unchecked

    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.nonEmpty)
    assert(schemas("anything_boolean") === Right(AnySchema.Anything))
    assert(schemas("nothing_boolean") === Right(AnySchema.Nothing))
  }

  test("spec any nothing schema object") {
    val Right(openapi) = readJson("/spec/3.1/any_and_nothing2.json").flatMap(_.as[OpenAPI]): @unchecked

    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.nonEmpty)
    assert(schemas("anything_object") === Right(AnySchema.Anything))
    assert(schemas("nothing_object") === Right(AnySchema.Nothing))
  }

  test("all schemas types") {
    val Right(openapi) = readJson("/spec/3.1/schema.json").flatMap(_.as[OpenAPI]): @unchecked
    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.nonEmpty)
    val Right(model) = schemas("model"): @unchecked
    assert(model.asInstanceOf[Schema].properties.size === 12)
  }
}
