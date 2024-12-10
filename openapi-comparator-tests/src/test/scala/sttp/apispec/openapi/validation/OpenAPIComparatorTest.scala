package sttp.apispec.openapi.validation

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.SchemaType
import sttp.apispec.openapi.ParameterStyle
import sttp.apispec.openapi.{OpenAPI, ResponsesCodeKey}
import sttp.apispec.openapi.circe.openAPIDecoder
import sttp.apispec.test.ResourcePlatform
import sttp.apispec.validation.TypeMismatch

import scala.collection.immutable.ListMap

class OpenAPIComparatorTest extends AnyFunSuite with ResourcePlatform {
  override val basedir = "openapi-comparator-tests"

  def readOpenAPI(path: String): OpenAPI = readJson(path).flatMap(_.as[OpenAPI]) match {
    case Right(openapi) => openapi
    case Left(error)    => throw new RuntimeException(s"Failed to parse OpenAPI from $path: $error")
  }

  private def compare(clientOpenapi: OpenAPI, serverOpenapi: OpenAPI): List[OpenAPICompatibilityIssue] =
    OpenAPIComparator(clientOpenapi, serverOpenapi).compare()

  test("identical") {
    val clientOpenapi = readOpenAPI("/petstore/identical/petstore-identical.json")
    val serverOpenapi = readOpenAPI("/petstore/identical/petstore.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("no errors when metadata is updated") {
    val clientOpenapi = readOpenAPI("/petstore/changed-metadata/petstore-changed-metadata.json")
    val serverOpenapi = readOpenAPI("/petstore/changed-metadata/petstore.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing path when client has an extra one") {
    val clientOpenapi = readOpenAPI("/petstore/added-path/petstore-added-path.json")
    val serverOpenapi = readOpenAPI("/petstore/added-path/petstore.json")

    val expected = List(MissingPath("/pets/{petId}"))

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional path") {
    val clientOpenapi = readOpenAPI("/petstore/added-path/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-path/petstore-added-path.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing operation when client has an extra one") {
    val clientOpenapi = readOpenAPI("/petstore/added-operation/petstore-added-operation.json")
    val serverOpenapi = readOpenAPI("/petstore/added-operation/petstore.json")

    val operationIssue = MissingOperation("post")
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional operation") {
    val clientOpenapi = readOpenAPI("/petstore/added-operation/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-operation/petstore-added-operation.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing parameter when client has an extra one") {
    val clientOpenapi = readOpenAPI("/petstore/added-parameter/petstore-added-parameter.json")
    val serverOpenapi = readOpenAPI("/petstore/added-parameter/petstore.json")

    val parameterIssue = MissingParameter("status")
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional parameter") {
    val clientOpenapi = readOpenAPI("/petstore/added-parameter/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-parameter/petstore-added-parameter.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server parameter schema is incompatible with client schema") {
    val clientOpenapi = readOpenAPI("/petstore/updated-parameter-schema/petstore-updated-parameter-schema.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-parameter-schema/petstore.json")

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.Array), List(SchemaType.String))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val parameterIssue = IncompatibleParameter("status", List(schemaIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing parameter schema when client has one") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-parameter-schema/petstore-added-parameter-schema.json")
    val serverOpenapi = readOpenAPI("/petstore/added-parameter-schema/petstore.json")

    val schemaIssue = MissingSchema()
    val parameterIssue = IncompatibleParameter("status", List(schemaIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has a parameter schema but client does not") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-parameter-schema/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-parameter-schema/petstore-added-parameter-schema.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing parameter content media-type when client has an extra one") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-parameter-content-mediatype/petstore-added-parameter-content-mediatype.json")
    val serverOpenapi = readOpenAPI("/petstore/added-parameter-content-mediatype/petstore.json")

    val mediaTypeIssue = MissingMediaType("application/json")
    val parameterContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("status", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional parameter content media-type") {
    val clientOpenapi = readOpenAPI("/petstore/added-parameter-content-mediatype/petstore.json")
    val serverOpenapi =
      readOpenAPI("/petstore/added-parameter-content-mediatype/petstore-added-parameter-content-mediatype.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server parameter content media-type schema is incompatible with client schema") {
    val clientOpenapi = readOpenAPI(
      "/petstore/updated-parameter-content-mediatype-schema/petstore-updated-parameter-content-mediatype-schema.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/updated-parameter-content-mediatype-schema/petstore.json")

    val schemaMismatch = IncompatibleSchema(List(TypeMismatch(List(SchemaType.String), List(SchemaType.Array))))
    val mediaTypeIssue = IncompatibleMediaType("application/json", List(schemaMismatch))
    val parameterContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("status", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing parameter content media-type schema when client has one") {
    val clientOpenapi =
      readOpenAPI(
        "/petstore/added-parameter-content-mediatype-schema/petstore-added-parameter-content-mediatype-schema.json"
      )
    val serverOpenapi = readOpenAPI("/petstore/added-parameter-content-mediatype-schema/petstore.json")

    val schemaIssue = MissingSchema()
    val mediaTypeIssue = IncompatibleMediaType("application/json", List(schemaIssue))
    val parameterContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("status", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has a parameter content media-type schema but client does not") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-parameter-content-mediatype-schema/petstore.json")
    val serverOpenapi = readOpenAPI(
      "/petstore/added-parameter-content-mediatype-schema/petstore-added-parameter-content-mediatype-schema.json"
    )

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server parameter style is incompatible with client parameter style") {
    val clientOpenapi = readOpenAPI("/petstore/updated-parameter-style/petstore-updated-parameter-style.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-parameter-style/petstore.json")

    val styleIssue = IncompatibleStyle(Some(ParameterStyle.Form), None)
    val parameterIssue = IncompatibleParameter("status", List(styleIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter explode is incompatible with client parameter explode") {
    val clientOpenapi =
      readOpenAPI("/petstore/updated-parameter-explode/petstore-updated-parameter-explode.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-parameter-explode/petstore.json")

    val explodeIssue = IncompatibleExplode(Some(true), None)
    val parameterIssue = IncompatibleParameter("status", List(explodeIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter allowEmptyValue is incompatible with client parameter allowEmptyValue") {
    val clientOpenapi =
      readOpenAPI("/petstore/updated-parameter-allow_empty_value/petstore-updated-parameter-allow_empty_value.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-parameter-allow_empty_value/petstore.json")

    val allowEmptyValueIssue = IncompatibleAllowEmptyValue(Some(true), None)
    val parameterIssue = IncompatibleParameter("status", List(allowEmptyValueIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter allowReserved is incompatible with client parameter allowReserved") {
    val clientOpenapi =
      readOpenAPI("/petstore/updated-parameter-allow_reserved/petstore-updated-parameter-allow_reserved.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-parameter-allow_reserved/petstore.json")

    val allowReservedIssue = IncompatibleAllowReserved(Some(true), None)
    val parameterIssue = IncompatibleParameter("status", List(allowReservedIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing request-body when client has an extra one") {
    val clientOpenapi = readOpenAPI("/petstore/added-requestbody/petstore-added-requestbody.json")
    val serverOpenapi = readOpenAPI("/petstore/added-requestbody/petstore.json")

    val requestBodyIssue = MissingRequestBody()
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional request-body") {
    val clientOpenapi = readOpenAPI("/petstore/added-requestbody/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-requestbody/petstore-added-requestbody.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing request-body content media-type when client has an extra one") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore-added-requestbody-content-mediatype.json")
    val serverOpenapi = readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore.json")

    val mediaTypeIssue = MissingMediaType("application/xml")
    val requestBodyContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val requestBodyIssue = IncompatibleRequestBody(List(requestBodyContentIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional request-body content media-type") {
    val clientOpenapi = readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore.json")
    val serverOpenapi =
      readOpenAPI("/petstore/added-requestbody-content-mediatype/petstore-added-requestbody-content-mediatype.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server request-body content media-type schema is incompatible with client schema") {
    val clientOpenapi = readOpenAPI(
      "/petstore/updated-requestbody-content-mediatype-schema/petstore-updated-requestbody-content-mediatype-schema.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/updated-requestbody-content-mediatype-schema/petstore.json")

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

  test("server missing request-body content media-type schema when client has one") {
    val clientOpenapi =
      readOpenAPI(
        "/petstore/added-requestbody-content-mediatype-schema/petstore-added-requestbody-content-mediatype-schema.json"
      )
    val serverOpenapi = readOpenAPI("/petstore/added-requestbody-content-mediatype-schema/petstore.json")

    val schemaIssue = MissingSchema()
    val mediaTypeIssue = IncompatibleMediaType("application/json", List(schemaIssue))
    val requestBodyContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val requestBodyIssue = IncompatibleRequestBody(List(requestBodyContentIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has a request-body content media-type schema but client does not") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-requestbody-content-mediatype-schema/petstore.json")
    val serverOpenapi = readOpenAPI(
      "/petstore/added-requestbody-content-mediatype-schema/petstore-added-requestbody-content-mediatype-schema.json"
    )

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing response when client has an extra one") {
    val clientOpenapi = readOpenAPI("/petstore/added-response/petstore-added-response.json")
    val serverOpenapi = readOpenAPI("/petstore/added-response/petstore.json")

    val responsesIssue = MissingResponse(ResponsesCodeKey(500))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional response") {
    val clientOpenapi = readOpenAPI("/petstore/added-response/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-response/petstore-added-response.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing response content media-type when client has an extra one") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-response-content-mediatype/petstore-added-response-content-mediatype.json")
    val serverOpenapi = readOpenAPI("/petstore/added-response-content-mediatype/petstore.json")

    val mediaTypeIssues = MissingMediaType("application/xml")
    val responseContentIssues = IncompatibleContent(List(mediaTypeIssues))
    val responsesIssue = IncompatibleResponse(List(responseContentIssues))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional response content media-type") {
    val clientOpenapi = readOpenAPI("/petstore/added-response-content-mediatype/petstore.json")
    val serverOpenapi =
      readOpenAPI("/petstore/added-response-content-mediatype/petstore-added-response-content-mediatype.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server response content media-type schema is incompatible with client schema") {
    val clientOpenapi = readOpenAPI(
      "/petstore/updated-response-content-mediatype-schema/petstore-updated-response-content-mediatype-schema.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/updated-response-content-mediatype-schema/petstore.json")

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

  test("server missing response content media-type schema when client has one") {
    val clientOpenapi =
      readOpenAPI(
        "/petstore/added-response-content-mediatype-schema/petstore-added-response-content-mediatype-schema.json"
      )
    val serverOpenapi = readOpenAPI("/petstore/added-response-content-mediatype-schema/petstore.json")

    val schemaIssue = MissingSchema()
    val mediaTypeIssues = IncompatibleMediaType("application/json", List(schemaIssue))
    val responseContentIssues = IncompatibleContent(List(mediaTypeIssues))
    val responsesIssue = IncompatibleResponse(List(responseContentIssues))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has a response content media-type schema but client does not") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-response-content-mediatype-schema/petstore.json")
    val serverOpenapi = readOpenAPI(
      "/petstore/added-response-content-mediatype-schema/petstore-added-response-content-mediatype-schema.json"
    )

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing response header when client has an extra one") {
    val clientOpenapi = readOpenAPI("/petstore/added-response-header/petstore-added-response-header.json")
    val serverOpenapi = readOpenAPI("/petstore/added-response-header/petstore.json")

    val headerIssue = MissingHeader("X-Rate-Limit")
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has an additional response header") {
    val clientOpenapi = readOpenAPI("/petstore/added-response-header/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-response-header/petstore-added-response-header.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server response header schema is incompatible with client schema") {
    val clientOpenapi =
      readOpenAPI("/petstore/updated-response-header-schema/petstore-updated-response-header-schema.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-response-header-schema/petstore.json")

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(schemaIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing response header schema when client has one") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-response-header-schema/petstore-added-response-header-schema.json")
    val serverOpenapi = readOpenAPI("/petstore/added-response-header-schema/petstore.json")

    val schemaIssue = MissingSchema()
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(schemaIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has a response header schema but client does not") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-response-header-schema/petstore.json")
    val serverOpenapi = readOpenAPI("/petstore/added-response-header-schema/petstore-added-response-header-schema.json")

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server missing response header content media-type when client has an extra one") {
    val clientOpenapi = readOpenAPI(
      "/petstore/added-response-header-content-mediatype/petstore-added-response-header-content-mediatype.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/added-response-header-content-mediatype/petstore.json")

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
    val clientOpenapi = readOpenAPI("/petstore/added-response-header-content-mediatype/petstore.json")
    val serverOpenapi = readOpenAPI(
      "/petstore/added-response-header-content-mediatype/petstore-added-response-header-content-mediatype.json"
    )

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server response header content media-type schema is incompatible with client schema") {
    val clientOpenapi = readOpenAPI(
      "/petstore/updated-response-header-content-mediatype-schema/petstore-updated-response-header-content-mediatype-schema.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/updated-response-header-content-mediatype-schema/petstore.json")

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

  test("server missing response header content media-type schema when client has one") {
    val clientOpenapi =
      readOpenAPI(
        "/petstore/added-response-header-content-mediatype-schema/petstore-added-response-header-content-mediatype-schema.json"
      )
    val serverOpenapi = readOpenAPI("/petstore/added-response-header-content-mediatype-schema/petstore.json")

    val schemaIssue = MissingSchema()
    val mediaTypeIssues = IncompatibleMediaType("application/json", List(schemaIssue))
    val contentIssues = IncompatibleContent(List(mediaTypeIssues))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(contentIssues))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has a response header content media-type schema but client does not") {
    val clientOpenapi =
      readOpenAPI("/petstore/added-response-header-content-mediatype-schema/petstore.json")
    val serverOpenapi = readOpenAPI(
      "/petstore/added-response-header-content-mediatype-schema/petstore-added-response-header-content-mediatype-schema.json"
    )

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test("server response header style is incompatible with client response header style") {
    val clientOpenapi =
      readOpenAPI("/petstore/updated-response-header-style/petstore-updated-response-header-style.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-response-header-style/petstore.json")

    val styleIssue = IncompatibleStyle(Some(ParameterStyle.Form), None)
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(styleIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header explode is incompatible with client response header explode") {
    val clientOpenapi =
      readOpenAPI("/petstore/updated-response-header-explode/petstore-updated-response-header-explode.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-response-header-explode/petstore.json")

    val explodeIssue = IncompatibleExplode(Some(true), None)
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(explodeIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header allowEmptyValue is incompatible with client response header allowEmptyValue") {
    val clientOpenapi = readOpenAPI(
      "/petstore/updated-response-header-allow_empty_value/petstore-updated-response-header-allow_empty_value.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/updated-response-header-allow_empty_value/petstore.json")

    val allowEmptyValueIssue = IncompatibleAllowEmptyValue(Some(true), None)
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(allowEmptyValueIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header allowReserved is incompatible with client response header allowReserved") {
    val clientOpenapi = readOpenAPI(
      "/petstore/updated-response-header-allow_reserved/petstore-updated-response-header-allow_reserved.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/updated-response-header-allow_reserved/petstore.json")

    val allowReservedIssue = IncompatibleAllowReserved(Some(true), None)
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(allowReservedIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter name is incompatible with client parameter name") {
    val clientOpenapi = readOpenAPI("/petstore/updated-parameter-name/petstore-updated-parameter-name.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-parameter-name/petstore.json")

    val expected = List(MissingPath("/pets/{Id}"))

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server parameter required value is incompatible with client parameter required value") {
    val clientOpenapi = readOpenAPI("/petstore/required-parameter/petstore-required-parameter.json")
    val serverOpenapi = readOpenAPI("/petstore/required-parameter/petstore.json")

    val requiredValueIssue = IncompatibleRequiredValue(Some(true), Some(false))
    val parameterIssue = IncompatibleParameter("petId", List(requiredValueIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server response header required value is incompatible with client response header required value") {
    val clientOpenapi = readOpenAPI("/petstore/required-response-header/petstore-required-response-header.json")
    val serverOpenapi = readOpenAPI("/petstore/required-response-header/petstore.json")

    val requiredValueIssue = IncompatibleRequiredValue(Some(true), Some(false))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(requiredValueIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server request-body required value is incompatible with client request-body required value") {
    val clientOpenapi = readOpenAPI("/petstore/required-request-body/petstore-required-request-body.json")
    val serverOpenapi = readOpenAPI("/petstore/required-request-body/petstore.json")

    val requiredValueIssue = IncompatibleRequiredValue(Some(true), Some(false))
    val requestBodyIssue = IncompatibleRequestBody(List(requiredValueIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server missing request-body content media-type encoding when client has one") {
    val clientOpenapi = readOpenAPI(
      "/petstore/added-requestbody-content-mediatype-encoding/petstore-added-requestbody-content-mediatype-encoding.json"
    )
    val serverOpenapi = readOpenAPI("/petstore/added-requestbody-content-mediatype-encoding/petstore.json")

    val encodingIssue = MissingEncoding("photo")
    val mediaTypeIssues = IncompatibleMediaType("multipart/form-data", List(encodingIssue))
    val contentIssues = IncompatibleContent(List(mediaTypeIssues))
    val requestBodyIssue = IncompatibleRequestBody(List(contentIssues))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("no errors when server has a request-body content media-type encoding but client does not") {
    val clientOpenapi = readOpenAPI("/petstore/added-requestbody-content-mediatype-encoding/petstore.json")
    val serverOpenapi = readOpenAPI(
      "/petstore/added-requestbody-content-mediatype-encoding/petstore-added-requestbody-content-mediatype-encoding.json"
    )

    assert(compare(clientOpenapi, serverOpenapi).isEmpty)
  }

  test(
    "server request-body content media-type encoding is incompatible with client request-body content media-type encoding"
  ) {
    val clientOpenapi =
      readOpenAPI(
        "/petstore/updated-requestbody-content-mediatype-encoding/petstore-updated-requestbody-content-mediatype-encoding.json"
      )
    val serverOpenapi = readOpenAPI("/petstore/updated-requestbody-content-mediatype-encoding/petstore.json")

    val missingHeaderIssue = MissingHeader("X-Custom-Header")
    val allowReservedIssue = IncompatibleAllowReserved(Some(false), None)
    val explodeIssue = IncompatibleExplode(Some(true), None)
    val styleIssue = IncompatibleStyle(Some(ParameterStyle.Form), None)
    val encodingIssue = IncompatibleEncoding(
      "photo",
      List(
        missingHeaderIssue,
        styleIssue,
        explodeIssue,
        allowReservedIssue
      )
    )
    val mediaTypeIssue = IncompatibleMediaType("multipart/form-data", List(encodingIssue))
    val contentIssue = IncompatibleContent(List(mediaTypeIssue))
    val requestBodyIssue = IncompatibleRequestBody(List(contentIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }

  test("server operation security is incompatible with client operation security") {
    val clientOpenapi = readOpenAPI("/petstore/updated-operation-security/petstore-updated-operation-security.json")
    val serverOpenapi = readOpenAPI("/petstore/updated-operation-security/petstore.json")

    val securityRequirementIssue =
      IncompatibleSecurityRequirement(ListMap("OAuth2" -> Vector("read:pets", "write:pets")))
    val operationIssue = IncompatibleOperation("get", List(securityRequirementIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(compare(clientOpenapi, serverOpenapi) == expected)
  }
}
