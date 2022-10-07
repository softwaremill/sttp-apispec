package sttp.apispec
package asyncapi
package circe

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ListMap
import io.circe.syntax._

class EncoderTest extends AnyFunSuite {
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

  def parse(s: String): Json = io.circe.parser.parse(s).fold(throw _, identity)
}
