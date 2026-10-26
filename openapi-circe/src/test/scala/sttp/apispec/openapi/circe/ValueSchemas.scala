package sttp.apispec.openapi.circe

import sttp.apispec._

import scala.collection.immutable.ListMap

/** In-code versions of the enum, const, default and example schemas from the `spec/3.x/schema.json` fixtures. */
object ValueSchemas {
  private def enumOf(tpe: SchemaType, values: Any*): Schema =
    Schema(tpe).copy(`enum` = Some(values.toList.map(ExampleSingleValue(_))))

  val all: ListMap[String, Schema] = ListMap(
    "integer enum" -> enumOf(SchemaType.Integer, 200, 404),
    "boolean enum" -> enumOf(SchemaType.Boolean, true),
    "string enum with JSON values" -> enumOf(SchemaType.String, "1", "true"),
    "nullable string enum" -> enumOf(SchemaType.String, "1").nullable,
    "string const" -> Schema(SchemaType.String).copy(const = Some(ExampleSingleValue("1"))),
    "string default and example" -> Schema(SchemaType.String)
      .copy(default = Some(ExampleSingleValue("1")), examples = Some(List(ExampleSingleValue("true")))),
    "object default and example" -> Schema(SchemaType.Object).copy(
      default = Some(ExampleSingleValue("{\"a\":1}")),
      examples = Some(List(ExampleSingleValue("{\"b\":[1,2]}")))
    )
  ).map { case (desc, schema) => desc -> schema.copy(description = Some(desc)) }
}
