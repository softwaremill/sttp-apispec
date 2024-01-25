package sttp.apispec.openapi.circe.threeone

import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec._
import sttp.apispec.openapi._
import sttp.apispec.openapi.circe.SttpOpenAPICirceEncoders
import sttp.apispec.test._

import scala.collection.immutable.ListMap

class EncoderTest extends AnyFunSuite with ResourcePlatform with SttpOpenAPICirceEncoders {
  override val basedir = "openapi-circe"

  val petstore: OpenAPI = OpenAPI(
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

  def arrayOf(s: SchemaLike) = Schema(`type` = Some(SchemaType.Array), items = Some(s))

  def ref(s: String): SchemaLike = Schema($ref = Some(s))

  test("petstore serialize") {
    val withPathItem = petstore.addPathItem(
      "/pets",
      PathItem(
        get = Some(
          Operation(
            operationId = Some("getPets"),
            description = Some("Gets all pets")
          ).addResponse(200, Response(
            description = "Success", 
            content = ListMap("application/json" ->
              MediaType(schema = Some(arrayOf(ref("#/components/schemas/Pet"))))
            )  
          ))
        )
      )
    )
    val petSchema = Schema(
        `type` = Some(SchemaType.Object), 
        properties = 
          ListMap("id" -> Schema(`type` = Some(SchemaType.Integer), format = Some("int32")),
            "name" -> Schema(`type` = Some(SchemaType.String))
          )
      )
    val withComponents = withPathItem.components(Components(schemas = ListMap(
      "Pet" -> petSchema
    )))

    val serialized = withComponents.asJson
    val Right(json) = readJson("/petstore/basic-petstore.json"): @unchecked

    assert(serialized === json)
  }

  test("full schema") {

    def schemaTypeAndDescription(desc: String, typ: SchemaType) =
      Schema(description = Some(desc), `type` = Some(typ))

    val components = Components(
      schemas = ListMap(
        "model" -> Schema(SchemaType.Object).copy(
          properties = ListMap(
            "one" -> schemaTypeAndDescription(
              "type array",
              ArraySchemaType(List(SchemaType.Integer, SchemaType.String))
            ),
            "two" -> schemaTypeAndDescription("type 'null'", SchemaType.Null),
            "three" -> schemaTypeAndDescription(
              "type array including 'null'",
              ArraySchemaType(List(SchemaType.String, SchemaType.Null))
            ),
            "four" -> schemaTypeAndDescription("array with no items", SchemaType.Array),
            "five" -> schemaTypeAndDescription("singular example", SchemaType.String)
              .copy(example = Some(ExampleSingleValue("exampleValue"))),
            "six" -> Schema(
              description = Some("exclusiveMinimum true"),
              exclusiveMinimum = Some(true),
              minimum = Some(BigDecimal(10))
            ),
            "seven" -> Schema(description = Some("exclusiveMinimum false"), minimum = Some(BigDecimal(10))),
            "eight" -> Schema(
              description = Some("exclusiveMaximum true"),
              exclusiveMaximum = Some(true),
              maximum = Some(BigDecimal(20))
            ),
            "nine" -> Schema(description = Some("exclusiveMaximum false"), maximum = Some(BigDecimal(20))),
            "ten" -> schemaTypeAndDescription("nullable string", SchemaType.String).copy(nullable = Some(true)),
            "eleven" -> schemaTypeAndDescription(
              "x-nullable string",
              ArraySchemaType(List(SchemaType.String, SchemaType.Null))
            ),
            "twelve" -> Schema(description = Some("file/binary"))
          )
        )
      )
    )

    val openapi = OpenAPI(
      info = Info(title = "API", version = "1.0.0"),
      components = Some(components)
    )

    val openApiJson = openapi.asJson
    val Right(json) = readJson("/spec/3.1/schema.json"): @unchecked

    assert(openApiJson.spaces2SortKeys == json.spaces2SortKeys)
  }

  test("encode security schema with empty scopes") {
    val Right(expectedSecuritySchema) = readJson("/securitySchema/security-schema-with-empty-scopes.json")

    val scopesRequirement: ListMap[String, String] = ListMap.empty[String, String]
    val clientCredentialsSecurityScheme: Option[SecurityScheme] =
      Some(
        SecurityScheme(
          `type` = "oauth2",
          description = None,
          name = None,
          in = None,
          scheme = None,
          bearerFormat = None,
          flows = Some(
            OAuthFlows(clientCredentials =
              Some(
                OAuthFlow(
                  authorizationUrl = None,
                  tokenUrl = Some("openapi-circe-token"),
                  refreshUrl = None,
                  scopes = scopesRequirement
                )
              )
            )
          ),
          openIdConnectUrl = None,
          extensions = ListMap.empty
        )
      )

    assert(expectedSecuritySchema === clientCredentialsSecurityScheme.asJson)
  }

  test("encode security schema with not empty scopes") {
    val Right(expectedSecuritySchema) = readJson("/securitySchema/security-schema-with-scopes.json")

    val scopesRequirement: ListMap[String, String] = ListMap("example" -> "description")
    val clientCredentialsSecurityScheme: Option[SecurityScheme] =
      Some(
        SecurityScheme(
          `type` = "oauth2",
          description = None,
          name = None,
          in = None,
          scheme = None,
          bearerFormat = None,
          flows = Some(
            OAuthFlows(clientCredentials =
              Some(
                OAuthFlow(
                  authorizationUrl = None,
                  tokenUrl = Some("openapi-circe-token"),
                  refreshUrl = None,
                  scopes = scopesRequirement
                )
              )
            )
          ),
          openIdConnectUrl = None,
          extensions = ListMap.empty
        )
      )

    assert(expectedSecuritySchema === clientCredentialsSecurityScheme.asJson)
  }
}
