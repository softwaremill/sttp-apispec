package sttp.apispec.openapi.validation

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.SchemaType
import sttp.apispec.openapi.{OpenAPI, ResponsesCodeKey}
import sttp.apispec.openapi.circe.openAPIDecoder
import sttp.apispec.test.ResourcePlatform
import sttp.apispec.validation.TypeMismatch

class OpenAPIComparatorTest extends AnyFunSuite with ResourcePlatform {
  override val basedir = "openapi-comparator-tests"

  test("identical") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_2_4.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_2_4.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    assert(openAPIComparator.compare().isEmpty)
  }

  test("missing path") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_2.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_1.json").flatMap(_.as[OpenAPI]): @unchecked
    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val expected = List(MissingPath("/pets/{petId}"))

    assert(openAPIComparator.compare() == expected)
  }

  test("missing operation") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_3.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_2.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val operationIssue = MissingOperation("post")
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("missing parameter") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_4.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_3.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val parameterIssue = MissingParameter("status")
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible parameter schema") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_5.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_4.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.Array), List(SchemaType.String))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val parameterIssue = IncompatibleParameter("status", List(schemaIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("parameter content is missing media type") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_6.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_5.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val mediaTypeIssue = MissingMediaType("application/json")
    val parameterContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("status", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible media type parameter content schema") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_7.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_6.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val schemaMismatch = IncompatibleSchema(List(TypeMismatch(List(SchemaType.String), List(SchemaType.Array))))
    val mediaTypeIssue = IncompatibleMediaType("application/json", List(schemaMismatch))
    val parameterContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("status", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible parameter style") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_8.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_7.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("style")
    val parameterIssue = IncompatibleParameter("status", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible parameter explode") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_0_9.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_8.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("explode")
    val parameterIssue = IncompatibleParameter("status", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible parameter allowEmptyValue") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_0.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_0_9.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("allowEmptyValue")
    val parameterIssue = IncompatibleParameter("status", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible parameter allowReserved") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_1.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_0.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("allowReserved")
    val parameterIssue = IncompatibleParameter("status", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

//  test("missing request body") {
//    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_2.json").flatMap(_.as[OpenAPI]): @unchecked
//    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_1.json").flatMap(_.as[OpenAPI]): @unchecked
//
//    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)
//
//    val requestBodyIssue = MissingRequestBody()
//    val operationIssue = IncompatibleOperation("get", List(requestBodyIssue))
//    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
//    val expected = List(pathIssue)
//
//    assert(openAPIComparator.compare() == expected)
//  }

  test("missing request body content media type") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_2.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_1.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val mediaTypeIssue = MissingMediaType("application/xml")
    val requestBodyContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val requestBodyIssue = IncompatibleRequestBody(List(requestBodyContentIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible request body content media type schema") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_3.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_2.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.String), List(SchemaType.Object))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val mediaTypeIssue = IncompatibleMediaType("application/json", List(schemaIssue))
    val requestBodyContentIssue = IncompatibleContent(List(mediaTypeIssue))
    val requestBodyIssue = IncompatibleRequestBody(List(requestBodyContentIssue))
    val operationIssue = IncompatibleOperation("post", List(requestBodyIssue))
    val pathIssue = IncompatiblePath("/pets", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("missing response") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_4.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_3.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val responsesIssue = MissingResponse(ResponsesCodeKey(500))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("missing response content media type") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_5.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_4.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val mediaTypeIssues = MissingMediaType("application/xml")
    val responseContentIssues = IncompatibleContent(List(mediaTypeIssues))
    val responsesIssue = IncompatibleResponse(List(responseContentIssues))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible response content media type schema") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_6.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_5.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.String), List(SchemaType.Object))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val mediaTypeIssues = IncompatibleMediaType("application/xml", List(schemaIssue))
    val responseContentIssues = IncompatibleContent(List(mediaTypeIssues))
    val responsesIssue = IncompatibleResponse(List(responseContentIssues))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("missing header") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_7.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_6.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val headerIssue = MissingHeader("X-Rate-Limit")
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible header schema") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_8.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_7.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(schemaIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("missing header content media type") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_1_9.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_8.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val mediaTypeIssues = MissingMediaType("application/json")
    val contentIssues = IncompatibleContent(List(mediaTypeIssues))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(contentIssues))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible header content media type schema") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_2_0.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_1_9.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.Integer), List(SchemaType.String))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val mediaTypeIssues = IncompatibleMediaType("application/json", List(schemaIssue))
    val contentIssues = IncompatibleContent(List(mediaTypeIssues))
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(contentIssues))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible header style") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_2_1.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_2_0.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("style")
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(missMatchIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible header explode") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_2_2.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_2_1.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("explode")
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(missMatchIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible header allowEmptyValue") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_2_3.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_2_2.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("allowEmptyValue")
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(missMatchIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible header allowReserved") {
    val Right(clientOpenapi) = readJson("/petstore/basic-petstore-v_1_2_4.json").flatMap(_.as[OpenAPI]): @unchecked
    val Right(serverOpenapi) = readJson("/petstore/basic-petstore-v_1_2_3.json").flatMap(_.as[OpenAPI]): @unchecked

    val openAPIComparator = new OpenAPIComparator(clientOpenapi, serverOpenapi)

    val missMatchIssue = MissMatch("allowReserved")
    val headerIssue = IncompatibleHeader("X-Rate-Limit", List(missMatchIssue))
    val responsesIssue = IncompatibleResponse(List(headerIssue))
    val operationIssue = IncompatibleOperation("get", List(responsesIssue))
    val pathIssue = IncompatiblePath("/pets/{petId}", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }
}
