package sttp.apispec.openapi.circe

import sttp.apispec._

import scala.collection.immutable.ListMap

/** In-code versions of the enum and const schemas from the `spec/3.x/schema.json` fixtures. */
object EnumSchemas {
  private def enumOf(tpe: SchemaType, values: Any*): Schema =
    Schema(tpe).copy(`enum` = Some(values.toList.map(ExampleSingleValue(_))))

  val all: ListMap[String, Schema] = ListMap(
    "integer enum" -> enumOf(SchemaType.Integer, 200, 404),
    "boolean enum" -> enumOf(SchemaType.Boolean, true),
    "string enum with JSON values" -> enumOf(SchemaType.String, "1", "true", "null"),
    "nullable string enum" -> enumOf(SchemaType.String, "1").nullable,
    "string const" -> Schema(SchemaType.String).copy(const = Some(ExampleSingleValue("1")))
  ).map { case (desc, schema) => desc -> schema.copy(description = Some(desc)) }
}
