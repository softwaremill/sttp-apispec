package sttp.apispec.openapi.validation

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.{Schema, SchemaType}
import sttp.apispec.openapi.{
  Info,
  MediaType,
  OpenAPI,
  Operation,
  Parameter,
  ParameterIn,
  ParameterStyle,
  PathItem,
  Paths
}
import sttp.apispec.validation.TypeMismatch

import scala.collection.immutable.ListMap

class OpenAPIComparatorTest extends AnyFunSuite {
  private val paths = Paths(pathItems = ListMap("/test" -> PathItem()))
  private val pathItem = PathItem()
  private val operation = Operation()
  private val parameter = Parameter("test", ParameterIn.Path, schema = None)
  private val mediaType = MediaType()

  test("missing path") {
    val readerOpenAPI = OpenAPI(info = Info("", ""))
    val writerOpenAPI = OpenAPI(info = Info("", ""), paths = paths)
    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val expected = List(MissingPath("/test"))
    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible path -> missing operation") {
    val readerOpenAPI = OpenAPI(info = Info("", ""), paths = paths.addPathItem("/test", pathItem))
    val writerOpenAPI = OpenAPI(info = Info("", ""), paths = paths.addPathItem("/test", pathItem.get(operation)))

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val operationIssue = MissingOperation("get")
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible path -> incompatible operation -> missing parameter") {
    val readerOpenAPI =
      OpenAPI(info = Info("", ""), paths = paths.addPathItem("/test", pathItem.get(operation)))

    val writerOpenAPI =
      OpenAPI(info = Info("", ""), paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter))))

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val parameterIssue = MissingParameter("test")
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible path -> incompatible operation -> incompatible parameter -> incompatible schema") {
    val readerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths =
          paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.schema(Schema(SchemaType.Integer)))))
      )
    val writerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths =
          paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.schema(Schema(SchemaType.String)))))
      )

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val schemaTypeMismatch = TypeMismatch(List(SchemaType.Integer), List(SchemaType.String))
    val schemaIssue = IncompatibleSchema(List(schemaTypeMismatch))
    val parameterIssue = IncompatibleParameter("test", List(schemaIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test(
    "incompatible path -> incompatible operation -> incompatible parameter -> incompatible content -> missing media type"
  ) {
    val readerOpenAPI =
      OpenAPI(info = Info("", ""), paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter))))
    val writerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths =
          paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.addMediaType("test", mediaType))))
      )

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val mediaTypeIssue = MissingMediaType("test")
    val parameterContentIssue = IncompatibleParameterContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("test", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test(
    "incompatible path -> incompatible operation -> incompatible parameter -> incompatible content -> incompatible media type -> incompatible schema"
  ) {
    val readerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem(
          "/test",
          pathItem.get(
            operation.addParameter(parameter.addMediaType("test", mediaType.schema(Schema(SchemaType.Integer))))
          )
        )
      )

    val writerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem(
          "/test",
          pathItem.get(
            operation.addParameter(parameter.addMediaType("test", mediaType.schema(Schema(SchemaType.String))))
          )
        )
      )

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val schemaMismatch = IncompatibleSchema(List(TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))))
    val mediaTypeIssue = IncompatibleMediaType("test", List(schemaMismatch))
    val parameterContentIssue = IncompatibleParameterContent(List(mediaTypeIssue))
    val parameterIssue = IncompatibleParameter("test", List(parameterContentIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible path -> incompatible operation -> incompatible parameter -> style miss match") {
    val readerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.style(ParameterStyle.Label))))
      )

    val writerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.style(ParameterStyle.Simple))))
      )

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val missMatchIssue = MissMatch("style")
    val parameterIssue = IncompatibleParameter("test", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible path -> incompatible operation -> incompatible parameter -> explode miss match") {
    val readerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.explode(false))))
      )

    val writerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.explode(true))))
      )

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val missMatchIssue = MissMatch("explode")
    val parameterIssue = IncompatibleParameter("test", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible path -> incompatible operation -> incompatible parameter -> allowEmptyValue miss match") {
    val readerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.allowEmptyValue(false))))
      )

    val writerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.allowEmptyValue(true))))
      )

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val missMatchIssue = MissMatch("allowEmptyValue")
    val parameterIssue = IncompatibleParameter("test", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }

  test("incompatible path -> incompatible operation -> incompatible parameter -> allowReserved miss match") {
    val readerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.allowReserved(false))))
      )

    val writerOpenAPI =
      OpenAPI(
        info = Info("", ""),
        paths = paths.addPathItem("/test", pathItem.get(operation.addParameter(parameter.allowReserved(true))))
      )

    val openAPIComparator = new OpenAPIComparator(writerOpenAPI, readerOpenAPI)

    val missMatchIssue = MissMatch("allowReserved")
    val parameterIssue = IncompatibleParameter("test", List(missMatchIssue))
    val operationIssue = IncompatibleOperation("get", List(parameterIssue))
    val pathIssue = IncompatiblePath("/test", List(operationIssue))
    val expected = List(pathIssue)

    assert(openAPIComparator.compare() == expected)
  }
}
