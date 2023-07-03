package sttp.apispec
package asyncapi
package circe

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ListMap
import io.circe.syntax._
import sttp.apispec.test.ResourcePlatform

class EncoderTest extends AnyFunSuite with ResourcePlatform {
  override def basedir: String = "asyncapi-circe"

  private val tokenUrl = basedir + "-token"

  test("encode as expected") {
    val expected =
      parse("""{
          |  "messages" : {
          |    "string" : {
          |      "payload" : {
          |        "type" : "string"
          |      },
          |      "contentType" : "text/plain"
          |    }
          |  }
          |}""".stripMargin)

    val comp = Components(messages =
      ListMap(
        "string" -> Right(
          SingleMessage(payload = Some(Right(Right(Schema(SchemaType.String)))), contentType = Some("text/plain"))
        )
      )
    )

    assert(expected === comp.asJson.deepDropNullValues)
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
                  tokenUrl = Some(tokenUrl),
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
                  tokenUrl = Some(tokenUrl),
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

  def parse(s: String): Json = io.circe.parser.parse(s).fold(throw _, identity)

}
