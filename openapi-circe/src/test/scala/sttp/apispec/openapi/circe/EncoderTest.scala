package sttp.apispec
package openapi
package circe
package threeone

import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.test._

import scala.collection.immutable.ListMap

class EncoderTest extends AnyFunSuite with ResourcePlatform with circe.SttpOpenAPI3_1CirceEncoders {
  override val basedir = "openapi-circe"

  val petstore: OpenAPI = OpenAPI(
    openapi = "3.1.0",
    info = Info(
      title = "Sample Pet Store App",
      summary = Some("A pet store manager."),
      description = Some("This is a sample server for a pet store."),
      termsOfService = Some("https://example.com/terms/"),
      contact = Some(
        Contact(
          name = Some("API Support"),
          url = Some("https://www.example.com/support"),
          email = Some("support@example.com")
        )
      ),
      license = Some(
        License(
          name = "Apache 2.0",
          url = Some("https://www.apache.org/licenses/LICENSE-2.0.html")
        )
      ),
      version = "1.0.1"
    )
  )

  def refOr[A](a: A): ReferenceOr[A] = Right(a)

  test("petstore serialize") {
    val withPathItem = petstore.addPathItem(
      "/pets",
      PathItem(
        get = Some(
          Operation(
            operationId = Some("getPets"),
            description = Some("Gets all pets")
          ).addResponse(200, Response(description = "Success"))
        )
      )
    )

    val serialized = withPathItem.asJson
    val Right(json) = readJson("/petstore/basic-petstore.json"): @unchecked

    assert(serialized === json)
  }

  test("full schema") {

    def schemaTypeAndDescription(desc: String, typ: SchemaType) =
      Schema(description = Some(desc), `type` = Some(typ))

    val components = Components(
      schemas = ListMap(
        "model" -> refOr(
          Schema(SchemaType.Object).copy(
            properties = ListMap(
              "one" -> refOr(
                schemaTypeAndDescription("type array", ArraySchemaType(List(SchemaType.Integer, SchemaType.String)))
              ),
              "two" -> refOr(schemaTypeAndDescription("type 'null'", SchemaType.Null)),
              "three" -> refOr(
                schemaTypeAndDescription(
                  "type array including 'null'",
                  ArraySchemaType(List(SchemaType.String, SchemaType.Null))
                )
              ),
              "four" -> refOr(schemaTypeAndDescription("array with no items", SchemaType.Array)),
              "five" -> refOr(
                schemaTypeAndDescription("singular example", SchemaType.String)
                  .copy(example = Some(ExampleSingleValue("exampleValue")))
              ),
              "six" -> refOr(
                Schema(
                  description = Some("exclusiveMinimum true"),
                  exclusiveMinimum = Some(true),
                  minimum = Some(BigDecimal(10))
                )
              ),
              "seven" -> refOr(Schema(description = Some("exclusiveMinimum false"), minimum = Some(BigDecimal(10)))),
              "eight" -> refOr(
                Schema(
                  description = Some("exclusiveMaximum true"),
                  exclusiveMaximum = Some(true),
                  maximum = Some(BigDecimal(20))
                )
              ),
              "nine" -> refOr(Schema(description = Some("exclusiveMaximum false"), maximum = Some(BigDecimal(20)))),
              "ten" -> refOr(
                schemaTypeAndDescription("nullable string", SchemaType.String).copy(nullable = Some(true))
              ),
              "eleven" -> refOr(
                schemaTypeAndDescription("x-nullable string", ArraySchemaType(List(SchemaType.String, SchemaType.Null)))
              ),
              "twelve" -> refOr(Schema(description = Some("file/binary")))
            )
          )
        )
      )
    )

    val openapi = OpenAPI(
      openapi = "3.1.0",
      info = Info(title = "API", version = "1.0.0"),
      components = Some(components)
    )

    val openApiJson = openapi.asJson
    val Right(json) = readJson("/spec/3.1/schema.json"): @unchecked

    assert(openApiJson.spaces2SortKeys == json.spaces2SortKeys)
  }
}
