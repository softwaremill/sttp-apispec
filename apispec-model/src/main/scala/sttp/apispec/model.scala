package sttp.apispec

import scala.collection.immutable.ListMap

/** A value of an example, default, enum or const, as written in the JSON or YAML document. */
sealed trait ExampleValue

/** @param value
  *   A `Boolean`, `null`, number (`Int`, `Long`, `Short`, `Float`, `Double`, `BigDecimal` or `BigInt`) or `String`.
  *   Other values are written as strings, using `toString`.
  *
  * A `String` is either plain text (e.g. a query parameter or XML), or a value encoded as JSON (e.g. `"200"`, `"\"a\""`
  * or `"{\"a\":1}"`). Text which is valid JSON is written as that JSON value. In a string [[Schema]], this only applies
  * to JSON strings and `null`.
  */
case class ExampleSingleValue(value: Any) extends ExampleValue

/** @param values
  *   Values, as in [[ExampleSingleValue]].
  */
case class ExampleMultipleValue(values: List[Any]) extends ExampleValue

case class Tag(
    name: String,
    description: Option[String] = None,
    externalDocs: Option[ExternalDocumentation] = None,
    extensions: ListMap[String, ExtensionValue] = ListMap.empty
)

case class ExternalDocumentation(
    url: String,
    description: Option[String] = None,
    extensions: ListMap[String, ExtensionValue] = ListMap.empty
)

case class ExtensionValue(value: String)

/** @see https://json-schema.org/understanding-json-schema/reference/regular_expressions.html */
final case class Pattern(value: String)
