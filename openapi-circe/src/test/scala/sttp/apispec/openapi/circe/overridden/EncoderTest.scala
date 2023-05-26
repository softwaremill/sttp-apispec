package sttp.apispec.openapi.circe.overridden

import io.circe.syntax._
import sttp.apispec._
import sttp.apispec.openapi._
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.ListMap
import sttp.apispec.test._

class EncoderTest extends AnyFunSuite with ResourcePlatform {
  val basedir = "openapi-circe"
  def refOr[A](a: A): ReferenceOr[A] = Right(a)

  test("any boolean") {
    object obj extends sttp.apispec.openapi.circe.SttpOpenAPI3_1CirceEncoders
    import obj._

    val components = Components(
      schemas = ListMap(
        "anything_boolean" -> refOr(AnySchema.Anything),
        "nothing_boolean" -> refOr(AnySchema.Nothing)
      )
    )

    val openapi = OpenAPI(
      openapi = "3.1.0",
      info = Info(title = "API", version = "1.0.0"),
      components = Some(components)
    )

    val openApiJson = openapi.asJson
    val Right(json) = readJson("/spec/3.1/any_and_nothing1.json"): @unchecked

    assert(openApiJson.spaces2SortKeys == json.spaces2SortKeys)
  }

  test("any object") {
    object obj extends sttp.apispec.openapi.circe.SttpOpenAPI3_1CirceEncoders {
      override val anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Object
    }

    import obj._

    val components = Components(
      schemas = ListMap(
        "anything_object" -> refOr(AnySchema.Anything),
        "nothing_object" -> refOr(AnySchema.Nothing)
      )
    )

    val openapi = OpenAPI(
      openapi = "3.1.0",
      info = Info(title = "API", version = "1.0.0"),
      components = Some(components)
    )

    val openApiJson = openapi.asJson
    val Right(json) = readJson("/spec/3.1/any_and_nothing2.json"): @unchecked

    assert(openApiJson.spaces2SortKeys == json.spaces2SortKeys)
  }

}
