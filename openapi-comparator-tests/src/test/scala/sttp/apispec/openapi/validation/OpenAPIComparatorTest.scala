package sttp.apispec.openapi.validation

import io.circe
import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.SchemaType
import sttp.apispec.openapi.{OpenAPI, ResponsesCodeKey}
import sttp.apispec.openapi.circe.openAPIDecoder
import sttp.apispec.test.ResourcePlatform
import sttp.apispec.validation.TypeMismatch

class OpenAPIComparatorTest extends AnyFunSuite with ResourcePlatform {
  override val basedir = "openapi-comparator-tests"

  def readOpenAPI(path: String): Either[circe.Error, OpenAPI] = readJson(path).flatMap(_.as[OpenAPI]): @unchecked
  private def compare(clientOpenapi: OpenAPI, serverOpenapi: OpenAPI): List[OpenAPICompatibilityIssue] =
    new OpenAPIComparator(clientOpenapi, serverOpenapi)
      .compare()

  test("identical") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/identical/petstore-identical.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/identical/petstore.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("no errors when metadata is updated") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/changed-metadata/petstore-changed-metadata.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/changed-metadata/petstore.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing path when client has an extra one") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-path/petstore-added-path.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-path/petstore.json")

    val expected = List(MissingPath("/pets/{petId}"))

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional path") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-path/petstore.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-path/petstore-added-path.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing operation when client has an extra one") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-operation/petstore-added-operation.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-operation/petstore.json")

    val operationIssue = MissingOperation("post")
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional operation") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-operation/petstore.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-operation/petstore-added-operation.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing parameter when client has an extra one") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-parameter/petstore-added-parameter.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-parameter/petstore.json")

    val parameterIssue = MissingParameter("status")
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional parameter") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-parameter/petstore.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-parameter/petstore-added-parameter.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server parameter schema is incompatible with client schema") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/updated-parameter-schema/petstore-updated-parameter-schema.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-parameter-schema/petstore.json")

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.Array), List(SchemaType.String))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val parameterIssue = IncompatibleParameter("status", List(schemaIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing parameter content media-type when client has an extra one") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/added-parameter-content-mediatype/petstore-added-parameter-content-mediatype.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-parameter-content-mediatype/petstore.json")

    val mediaTypeIssue = MissingMediaType("application/json")
    val parameterContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("status", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional parameter content media-type") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-parameter-content-mediatype/petstore.json")
    val Right(serverOpenapi) =
      readOpenAPI("/petstore/added-parameter-content-mediatype/petstore-added-parameter-content-mediatype.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server parameter content media-type schema is incompatible with client schema") {
    val Right(clientOpenapi) = readOpenAPI(
      "/petstore/updated-parameter-content-mediatype-schema/petstore-updated-parameter-content-mediatype-schema.json"
    )
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-parameter-content-mediatype-schema/petstore.json")

    val schemaMismatch = IncompatibleSchema(List(TypeMismatch(List(SchemaType.String), List(SchemaType.Array))))
    val mediaTypeIssue = IncompatibleMediaType("application/json", List(schemaMismatch))
    val parameterContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("status", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter style is incompatible with client parameter style") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/updated-parameter-style/petstore-updated-parameter-style.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-parameter-style/petstore.json")

    val styleIssue = IncompatibleStyle()
    val parameterIssue = IncompatibleParameter("status", List(styleIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter explode is incompatible with client parameter explode") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/updated-parameter-explode/petstore-updated-parameter-explode.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-parameter-explode/petstore.json")

    val explodeIssue = IncompatibleExplode()
    val parameterIssue = IncompatibleParameter("status", List(explodeIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter allowEmptyValue is incompatible with client parameter allowEmptyValue") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/updated-parameter-allow_empty_value/petstore-updated-parameter-allow_empty_value.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-parameter-allow_empty_value/petstore.json")

    val allowEmptyValueIssue = IncompatibleAllowEmptyValue()
    val parameterIssue = IncompatibleParameter("status", List(allowEmptyValueIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter allowReserved is incompatible with client parameter allowReserved") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/updated-parameter-allow_reserved/petstore-updated-parameter-allow_reserved.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-parameter-allow_reserved/petstore.json")

    val allowReservedIssue = IncompatibleAllowReserved()
    val parameterIssue = IncompatibleParameter("status", List(allowReservedIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing request-body when client has an extra one") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-requestbody/petstore-added-requestbody.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-requestbody/petstore.json")

    val requestBodyIssue = MissingRequestBody()
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional request-body") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-requestbody/petstore.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-requestbody/petstore-added-requestbody.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing request-body content media-type when client has an extra one") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore-added-requestbody-content-mediatype.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore.json")

    val mediaTypeIssue = MissingMediaType("application/xml")
    val requestBodyContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val requestBodyIssue = IncompatibleRequestBody(List(requestBodyContentIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional request-body content media-type") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore.json")
    val Right(serverOpenapi) =
      readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore-added-requestbody-content-mediatype.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server request-body content media-type schema is incompatible with client schema") {
    val Right(clientOpenapi) = readOpenAPI(
      "/petstore/updated-requestbody-content-mediatype-schema/petstore-updated-requestbody-content-mediatype-schema.json"
    )
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-requestbody-content-mediatype-schema/petstore.json")

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.String), List(SchemaType.Object))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val mediaTypeIssue = IncompatibleMediaType("application/json", List(schemaIssue))
    val requestBodyContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val requestBodyIssue = IncompatibleRequestBody(List(requestBodyContentIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing response when client has an extra one") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-response/petstore-added-response.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-response/petstore.json")

    val responsesIssue = MissingResponse(ResponsesCodeKey(500))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional response") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-response/petstore.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-response/petstore-added-response.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing response content media-type when client has an extra one") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/added-response-content-mediatype/petstore-added-response-content-mediatype.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-response-content-mediatype/petstore.json")

    val mediaTypeIssues = MissingMediaType("application/xml")
    val responseContentIssues = IncompatibleContent(List(mediaTypeIssues))
    val responsesIssue = IncompatibleResponse(List(responseContentIssues))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional response content media-type") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-response-content-mediatype/petstore.json")
    val Right(serverOpenapi) =
      readOpenAPI("/petstore/added-response-content-mediatype/petstore-added-response-content-mediatype.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server response content media-type schema is incompatible with client schema") {
    val Right(clientOpenapi) = readOpenAPI(
      "/petstore/updated-response-content-mediatype-schema/petstore-updated-response-content-mediatype-schema.json"
    )
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-response-content-mediatype-schema/petstore.json")

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.String), List(SchemaType.Object))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val mediaTypeIssues = IncompatibleMediaType("application/xml", List(schemaIssue))
    val responseContentIssues = IncompatibleContent(List(mediaTypeIssues))
    val responsesIssue = IncompatibleResponse(List(responseContentIssues))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing response header when client has an extra one") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-response-header/petstore-added-response-header.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-response-header/petstore.json")

    val headerIssue = MissingHeader("X-Rate-Limit")
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional response header") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-response-header/petstore.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-response-header/petstore-added-response-header.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server response header schema is incompatible with client schema") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/updated-response-header-schema/petstore-updated-response-header-schema.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-response-header-schema/petstore.json")

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(schemaIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing response header content media-type when client has an extra one") {
    val Right(clientOpenapi) = readOpenAPI(
      "/petstore/added-response-header-content-mediatype/petstore-added-response-header-content-mediatype.json"
    )
    val Right(serverOpenapi) = readOpenAPI("/petstore/added-response-header-content-mediatype/petstore.json")

    val mediaTypeIssues = MissingMediaType("application/json")
    val contentIssues = IncompatibleContent(List(mediaTypeIssues))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(contentIssues))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional response header content media-type") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/added-response-header-content-mediatype/petstore.json")
    val Right(serverOpenapi) = readOpenAPI(
      "/petstore/added-response-header-content-mediatype/petstore-added-response-header-content-mediatype.json"
    )

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server response header content media-type schema is incompatible with client schema") {
    val Right(clientOpenapi) = readOpenAPI(
      "/petstore/updated-response-header-content-mediatype-schema/petstore-updated-response-header-content-mediatype-schema.json"
    )
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-response-header-content-mediatype-schema/petstore.json")

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.Integer), List(SchemaType.String))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val mediaTypeIssues = IncompatibleMediaType("application/json", List(schemaIssue))
    val contentIssues = IncompatibleContent(List(mediaTypeIssues))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(contentIssues))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header style is incompatible with client response header style") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/updated-response-header-style/petstore-updated-response-header-style.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-response-header-style/petstore.json")

    val styleIssue = IncompatibleStyle()
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(styleIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header explode is incompatible with client response header explode") {
    val Right(clientOpenapi) =
      readOpenAPI("/petstore/updated-response-header-explode/petstore-updated-response-header-explode.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-response-header-explode/petstore.json")

    val explodeIssue = IncompatibleExplode()
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(explodeIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header allowEmptyValue is incompatible with client response header allowEmptyValue") {
    val Right(clientOpenapi) = readOpenAPI(
      "/petstore/updated-response-header-allow_empty_value/petstore-updated-response-header-allow_empty_value.json"
    )
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-response-header-allow_empty_value/petstore.json")

    val allowEmptyValueIssue = IncompatibleAllowEmptyValue()
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(allowEmptyValueIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header allowReserved is incompatible with client response header allowReserved") {
    val Right(clientOpenapi) = readOpenAPI(
      "/petstore/updated-response-header-allow_reserved/petstore-updated-response-header-allow_reserved.json"
    )
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-response-header-allow_reserved/petstore.json")

    val allowReservedIssue = IncompatibleAllowReserved()
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(allowReservedIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter name is incompatible with client parameter name") {
    val Right(clientOpenapi) = readOpenAPI("/petstore/updated-parameter-name/petstore-updated-parameter-name.json")
    val Right(serverOpenapi) = readOpenAPI("/petstore/updated-parameter-name/petstore.json")

    val expected = List(MissingPath("/pets/{Id}"))

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }
}
