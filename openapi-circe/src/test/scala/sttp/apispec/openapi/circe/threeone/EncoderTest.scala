package sttp.apispec.openapi.circe.threeone

import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec._
import sttp.apispec.openapi._
import sttp.apispec.openapi.circe.SttpOpenAPICirceEncoders
import sttp.apispec.test._

import scala.collection.immutable.ListMap

class EncoderTest extends AnyFunSuite with ResourcePlatform {
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

  def arrayOf(s: SchemaLike) = Schema(`type` = Some(List(SchemaType.Array)), items = Some(s))

  def ref(s: String): SchemaLike = Schema($ref = Some(s))

  test("petstore serialize") {
    import sttp.apispec.openapi.circe._

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
      `type` = Some(List(SchemaType.Object)),
      properties =
        ListMap("id" -> Schema(`type` = Some(List(SchemaType.Integer)), format = Some("int32")),
          "name" -> Schema(`type` = Some(List(SchemaType.String)))
        )
    )
    val withComponents = withPathItem.components(Components(schemas = ListMap(
      "Pet" -> petSchema
    )))
    val server = Server(url = "http://petstore.swagger.io/v1")
    val withServer = withComponents.servers(List(server))
    val serialized = withServer.asJson
    val Right(json) = readJson("/petstore/basic-petstore.json"): @unchecked

    assert(serialized === json)
  }

  private def schemaComponent(desc: String)(schema: Schema): (String, Schema) =
    desc -> schema.copy(description = Some(desc))

  private val fullSchemaOpenApi = {
    val components = Components(
      schemas = ListMap(
        schemaComponent("type 'null'")(Schema(SchemaType.Null)),
        schemaComponent("nullable string")(Schema(SchemaType.String, SchemaType.Null)),
        schemaComponent("nullable reference")(Schema.referenceTo("#/components/schemas/", "Foo").nullable),
        schemaComponent("single example")(Schema(SchemaType.String)
          .copy(examples = Some(List(ExampleValue.string("exampleValue"))))),
        schemaComponent("min/max")(Schema(
          minimum = Some(BigDecimal(10)),
          maximum = Some(BigDecimal(20)),
        )),
        schemaComponent("exclusive min/max")(Schema(
          exclusiveMinimum = Some(BigDecimal(10)),
          exclusiveMaximum = Some(BigDecimal(20)),
        )),
        schemaComponent("exclusiveMinimum false")(Schema(minimum = Some(BigDecimal(10)))),
        schemaComponent("array")(Schema(SchemaType.Array).copy(items = Some(Schema(SchemaType.String)))),
        schemaComponent("array with unique items")(Schema(SchemaType.Array).copy(uniqueItems = Some(true))),
      )
    )

    OpenAPI(
      info = Info(title = "API", version = "1.0.0"),
      components = Some(components)
    )
  }

  test("full 3.1 schema") {
    import sttp.apispec.openapi.circe._

    val schemas31 = ListMap(
      schemaComponent("multiple examples")(Schema(SchemaType.String)
        .copy(examples = Some(List("ex1", "ex2").map(ExampleValue.string)))),
    )

    val openApiJson = fullSchemaOpenApi.copy(
      components = fullSchemaOpenApi.components.map(c => c.copy(schemas = c.schemas ++ schemas31))
    ).asJson
    val Right(json) = readJson("/spec/3.1/schema.json"): @unchecked

    println(openApiJson)

    assert(openApiJson.spaces2SortKeys == json.spaces2SortKeys)
  }

  test("full 3.0 schema") {
    import sttp.apispec.openapi.circe_openapi_3_0_3._

    val openApiJson = fullSchemaOpenApi.copy(openapi = "3.0.1").asJson
    val Right(json) = readJson("/spec/3.0/schema.json"): @unchecked

    println(openApiJson)

    assert(openApiJson.spaces2SortKeys == json.spaces2SortKeys)
  }

  private def clientCredentialsSecurityScheme(scopesRequirement: ListMap[String, String]): SecurityScheme =
    SecurityScheme(
      `type` = "oauth2",
      flows = Some(
        OAuthFlows(clientCredentials =
          Some(
            OAuthFlow(
              tokenUrl = Some("openapi-circe-token"),
              scopes = scopesRequirement
            )
          )
        )
      ),
    )


  test("encode security schema with empty scopes") {
    import sttp.apispec.openapi.circe._

    val Right(expectedSecuritySchema) = readJson("/securitySchema/security-schema-with-empty-scopes.json")
    val securityScheme = Some(clientCredentialsSecurityScheme(ListMap.empty))
    assert(expectedSecuritySchema === securityScheme.asJson)
  }

  test("encode security schema with not empty scopes") {
    import sttp.apispec.openapi.circe._

    val Right(expectedSecuritySchema) = readJson("/securitySchema/security-schema-with-scopes.json")
    val securityScheme = Some(clientCredentialsSecurityScheme(ListMap("example" -> "description")))
    assert(expectedSecuritySchema === securityScheme.asJson)
  }
}
