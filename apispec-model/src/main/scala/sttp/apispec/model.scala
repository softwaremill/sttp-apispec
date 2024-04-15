package sttp.apispec

import scala.collection.immutable.ListMap

sealed trait ExampleValue
case class ExampleSingleValue(value: Any) extends ExampleValue
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
