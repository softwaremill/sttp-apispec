package sttp.apispec
package openapi
package circe

import sttp.apispec.test._
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.ListMap

class DecoderTest extends AnyFunSuite with ResourcePlatform {
  override val basedir = "openapi-circe"

  def extractOrThrow[T](option: Option[T], errorMessage: String): T =
    option.getOrElse(throw new IllegalArgumentException(errorMessage))

  test("petstore deserialize") {
    val Right(openapi) = readJson("/petstore/basic-petstore.json").flatMap(_.as[OpenAPI]): @unchecked

    assert(openapi.info.description === Some("This is a sample server for a pet store."))
  }

  test("spec any nothing schema boolean") {
    val Right(openapi) = readJson("/spec/3.1/any_and_nothing1.json").flatMap(_.as[OpenAPI]): @unchecked

    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.nonEmpty)
    assert(schemas("anything_boolean") === AnySchema.Anything)
    assert(schemas("nothing_boolean") === AnySchema.Nothing)
  }

  test("spec any nothing schema object") {
    val Right(openapi) = readJson("/spec/3.1/any_and_nothing2.json").flatMap(_.as[OpenAPI]): @unchecked

    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.nonEmpty)
    assert(schemas("anything_object") === AnySchema.Anything)
    assert(schemas("nothing_object") === AnySchema.Nothing)
  }

  test("all schemas types 3.1") {
    val Right(openapi) = readJson("/spec/3.1/schema.json").flatMap(_.as[OpenAPI]): @unchecked
    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.size === 11)
  }

  test("all schemas types 3.0") {
    val Right(openapi) = readJson("/spec/3.0/schema.json").flatMap(_.as[OpenAPI]): @unchecked
    assert(openapi.info.title === "API")
    val schemas = openapi.components.getOrElse(Components.Empty).schemas
    assert(schemas.size === 10)
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

  test("should decode callbacks pathitems successfully") {
    val Right(openapi) = readJson("/callbacks/callbacks.json").flatMap(_.as[OpenAPI]): @unchecked

    val pathItem =
      extractOrThrow(openapi.paths.pathItems.get("/pets"), "The specified path item '/pets' does not exist")
    val operation = extractOrThrow(pathItem.post, "The POST operation is not defined for the path '/pets'")
    val callback = extractOrThrow(
      operation.callbacks.get("onPetStatusChange"),
      "The callback 'onPetStatusChange' is not defined for the POST operation."
    )

    val doesPathItemExist = callback.getOrElse(null).pathItems.contains("{$request.body#/callbackUrl}")

    assert(doesPathItemExist)
  }
}
