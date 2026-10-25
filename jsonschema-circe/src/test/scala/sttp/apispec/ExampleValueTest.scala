package sttp.apispec

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.circe._
import sttp.apispec.validation.SchemaComparator

class ExampleValueTest extends AnyFunSuite {
  private def decodeSchema(json: String): Schema = decode[Schema](json).fold(e => fail(e.toString), identity)
  private def compare(writer: Schema, reader: Schema) =
    new SchemaComparator(Map.empty[String, Schema], Map.empty[String, Schema]).compare(writer, reader)
  private def enumSchema(tpe: SchemaType, values: Any*): Schema =
    Schema(`type` = Some(List(tpe)), `enum` = Some(values.toList.map(ExampleSingleValue(_))))

  test("decoded scalar enum values are equal to in-code values") {
    val cases = List(
      """{"type":"integer","enum":[200,404]}""" -> enumSchema(SchemaType.Integer, 200, 404),
      """{"type":"integer","enum":[3000000000]}""" -> enumSchema(SchemaType.Integer, 3000000000L),
      """{"type":"number","enum":[1.5]}""" -> enumSchema(SchemaType.Number, 1.5),
      """{"type":"boolean","enum":[true]}""" -> enumSchema(SchemaType.Boolean, true),
      """{"type":"string","enum":["a"]}""" -> enumSchema(SchemaType.String, "a")
    )
    cases.foreach { case (json, inCode) =>
      val decoded = decodeSchema(json)
      assert(compare(decoded, inCode) == Nil, json)
      assert(compare(inCode, decoded) == Nil, json)
    }
  }

  test("decoded nullable enum is equal to in-code nullable enum") {
    val inCode = enumSchema(SchemaType.String, "a").nullable
    assert(compare(decodeSchema("""{"type":["string","null"],"enum":["a",null]}"""), inCode) == Nil)
    assert(compare(decodeSchema("""{"type":"string","nullable":true,"enum":["a"]}"""), inCode) == Nil)
  }

  test("decoding a mixed list keeps all values") {
    val decoded = decodeSchema("""{"examples":[["a",1,true,null]]}""")
    assert(decoded.examples == Some(List(ExampleMultipleValue(List("a", 1, true, "null")))))
  }

  test("decode then encode keeps example values") {
    val json = """{"examples":[200,true,{"a":1},[1,{"b":2}]],"enum":[1,"x",null]}"""
    assert(decodeSchema(json).asJson == io.circe.parser.parse(json).toOption.get)
  }

  test("string enum values are encoded as strings") {
    val schema = enumSchema(SchemaType.String, "1", "true", "\"quoted\"").nullable
    assert(
      schema.asJson.hcursor.get[Json]("enum") == Right(Json.arr("1".asJson, "true".asJson, "quoted".asJson, Json.Null))
    )
  }

  test("string const value is encoded as a string") {
    val schema = Schema(`type` = Some(List(SchemaType.String)), const = Some(ExampleSingleValue("1")))
    assert(schema.asJson.hcursor.get[Json]("const") == Right("1".asJson))
  }

  test("non-string enum values given as strings are parsed") {
    val schema = enumSchema(SchemaType.Integer, "200")
    assert(schema.asJson.hcursor.get[Json]("enum") == Right(Json.arr(200.asJson)))
  }
}
