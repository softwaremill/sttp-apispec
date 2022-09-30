package sttp.apispec

import scala.collection.immutable.ListMap

sealed trait SchemaLike

sealed trait AnySchema extends SchemaLike

object AnySchema {
  sealed trait Encoding extends Product with Serializable
  object Encoding {
    case object Object extends Encoding
    case object Boolean extends Encoding
  }

  case object Anything extends AnySchema
  case object Nothing extends AnySchema
}

// todo: xml
case class Schema(
    $schema: Option[String] = None,
    allOf: List[ReferenceOr[SchemaLike]] = List.empty,
    title: Option[String] = None,
    required: List[String] = List.empty,
    `type`: Option[SchemaType] = None,
    prefixItems: Option[List[ReferenceOr[SchemaLike]]] = None,
    items: Option[ReferenceOr[SchemaLike]] = None,
    contains: Option[ReferenceOr[SchemaLike]] = None,
    properties: ListMap[String, ReferenceOr[SchemaLike]] = ListMap.empty,
    patternProperties: ListMap[Pattern, ReferenceOr[SchemaLike]] = ListMap.empty,
    description: Option[String] = None,
    format: Option[String] = None,
    default: Option[ExampleValue] = None,
    nullable: Option[Boolean] = None,
    readOnly: Option[Boolean] = None,
    writeOnly: Option[Boolean] = None,
    example: Option[ExampleValue] = None,
    deprecated: Option[Boolean] = None,
    oneOf: List[ReferenceOr[SchemaLike]] = List.empty,
    discriminator: Option[Discriminator] = None,
    additionalProperties: Option[ReferenceOr[SchemaLike]] = None,
    pattern: Option[Pattern] = None,
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    minimum: Option[BigDecimal] = None,
    exclusiveMinimum: Option[Boolean] = None,
    maximum: Option[BigDecimal] = None,
    exclusiveMaximum: Option[Boolean] = None,
    minItems: Option[Int] = None,
    maxItems: Option[Int] = None,
    `enum`: Option[List[ExampleSingleValue]] = None,
    not: Option[ReferenceOr[SchemaLike]] = None,
    `if`: Option[ReferenceOr[SchemaLike]] = None,
    `then`: Option[ReferenceOr[SchemaLike]] = None,
    `else`: Option[ReferenceOr[SchemaLike]] = None,
    $defs: Option[ListMap[String, SchemaLike]] = None,
    extensions: ListMap[String, ExtensionValue] = ListMap.empty
) extends SchemaLike

case class Discriminator(propertyName: String, mapping: Option[ListMap[String, String]])

object Schema {
  def apply(schemaType: SchemaType): Schema = new Schema(`type` = Some(schemaType))

  def apply(references: List[ReferenceOr[Schema]], discriminator: Option[Discriminator]): Schema =
    new Schema(oneOf = references, discriminator = discriminator)
}

sealed trait SchemaType

final case class ArraySchemaType(value: List[BasicSchemaType]) extends SchemaType

sealed abstract class BasicSchemaType(val value: String) extends SchemaType

object SchemaType {
  case object Boolean extends BasicSchemaType("boolean")
  case object Object extends BasicSchemaType("object")
  case object Array extends BasicSchemaType("array")
  case object Number extends BasicSchemaType("number")
  case object String extends BasicSchemaType("string")
  case object Integer extends BasicSchemaType("integer")
  case object Null extends BasicSchemaType("null")
}

object SchemaFormat {
  val Int32: Option[String] = Some("int32")
  val Int64: Option[String] = Some("int64")
  val Float: Option[String] = Some("float")
  val Double: Option[String] = Some("double")
  val Byte: Option[String] = Some("byte")
  val Binary: Option[String] = Some("binary")
  val Date: Option[String] = Some("date")
  val DateTime: Option[String] = Some("date-time")
  val Password: Option[String] = Some("password")
}
