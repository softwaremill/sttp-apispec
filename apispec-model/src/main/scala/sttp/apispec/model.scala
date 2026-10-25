package sttp.apispec

import scala.collection.immutable.ListMap

/** A JSON value, used in examples, defaults, enums and consts. */
sealed trait ExampleValue

/** @param value
  *   A JSON value: a number (`Int`, `Long`, `Short`, `Float`, `Double`, `BigDecimal`, `BigInt`), a `Boolean`, `null`, a
  *   `String`, or an `io.circe.Json` (decoded arrays and objects). Other values are written as strings, using
  *   `toString`.
  *
  * A `String` is either a string, or an encoded JSON value:
  *   - in the `enum`, `const`, `default` and `examples` of a [[Schema]] with type `string` (optionally also `null`), it
  *     is a string, unless it's a JSON-encoded string or `null` (e.g. `"\"a\""` or `"null"`)
  *   - everywhere else, it's an encoded JSON value if it parses as JSON (e.g. `"200"` is a number), and a string
  *     otherwise
  *
  * Decoded values hold `Int`, `Long` or `BigDecimal` numbers, so a value compares equal to the same number given as
  * another numeric type, but not to the number given as a `String`.
  */
case class ExampleSingleValue(value: Any) extends ExampleValue

/** @param values
  *   JSON values, as in [[ExampleSingleValue]].
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
