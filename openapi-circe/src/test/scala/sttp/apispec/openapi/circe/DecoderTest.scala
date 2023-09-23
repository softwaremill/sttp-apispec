package sttp.apispec
package openapi
package circe

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.test._

import scala.collection.immutable.ListMap

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

  test("decode security schema with not empty scopes") {
    val expectedScopes = Some(Some(ListMap("example" -> "description")))
    val expectedToken = Some(Some(Some("openapi-circe-token")))

    val Right(securityScheme) =
      readJson("/securitySchema/security-schema-with-scopes.json").flatMap(_.as[SecurityScheme]): @unchecked

    assert(securityScheme.flows.map(_.clientCredentials.map(_.tokenUrl)) === expectedToken)
    assert(securityScheme.flows.map(_.clientCredentials.map(_.scopes)) === expectedScopes)
  }

  test("decode security schema with empty scopes") {
    val expectedScopes = Some(Some(ListMap.empty[String, String]))
    val expectedToken = Some(Some(Some("openapi-circe-token")))

    val Right(securityScheme) =
      readJson("/securitySchema/security-schema-with-empty-scopes.json").flatMap(_.as[SecurityScheme]): @unchecked

    assert(securityScheme.flows.map(_.clientCredentials.map(_.tokenUrl)) === expectedToken)
    assert(securityScheme.flows.map(_.clientCredentials.map(_.scopes)) === expectedScopes)
  }
}
