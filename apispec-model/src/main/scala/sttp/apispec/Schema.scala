package sttp.apispec

import scala.collection.immutable.ListMap

/** Algebraic data type for all possible schemas
  */
sealed trait SchemaLike

sealed trait AnySchema extends SchemaLike

object AnySchema {
  sealed trait Encoding extends Product with Serializable
  object Encoding {
    case object Object extends Encoding
    case object Boolean extends Encoding
  }

  /** Json schema can be represented by the values `true` or `{}` (empty object). This represents any json value. */
  case object Anything extends AnySchema

  /** Json schema can be represented by the values `false` or `{"not": {}}` (object with a single property "not" which
    * has a single value with must be the empty object". This represents no json value.
    */
  case object Nothing extends AnySchema
}

// todo: xml
case class Schema(
    // Core JSON Schema keywords
    // https://json-schema.org/draft/2020-12/json-schema-core#section-8
    $schema: Option[String] = None,
    $vocabulary: Option[ListMap[String, Boolean]] = None,
    $id: Option[String] = None,
    $anchor: Option[String] = None,
    $dynamicAnchor: Option[String] = None,
    $ref: Option[String] = None,
    $dynamicRef: Option[String] = None,
    $comment: Option[String] = None,
    $defs: Option[ListMap[String, SchemaLike]] = None,

    // Annotations
    // https://json-schema.org/draft/2020-12/json-schema-validation#section-9
    title: Option[String] = None,
    description: Option[String] = None,
    default: Option[ExampleValue] = None,
    deprecated: Option[Boolean] = None,
    readOnly: Option[Boolean] = None,
    writeOnly: Option[Boolean] = None,
    examples: Option[List[ExampleValue]] = None,

    // General assertions
    // https://json-schema.org/draft/2020-12/json-schema-validation#section-6.1
    `type`: Option[List[SchemaType]] = None,
    `enum`: Option[List[ExampleValue]] = None,
    const: Option[ExampleValue] = None,

    // https://json-schema.org/draft/2020-12/json-schema-validation#section-7
    format: Option[String] = None,

    // Logical applicators
    // https://json-schema.org/draft/2020-12/json-schema-core#section-10.2.1
    allOf: List[SchemaLike] = List.empty,
    anyOf: List[SchemaLike] = List.empty,
    oneOf: List[SchemaLike] = List.empty,
    not: Option[SchemaLike] = None,

    // Conditional applicators
    // https://json-schema.org/draft/2020-12/json-schema-core#section-10.2.2
    `if`: Option[SchemaLike] = None,
    `then`: Option[SchemaLike] = None,
    `else`: Option[SchemaLike] = None,
    dependentSchemas: ListMap[String, SchemaLike] = ListMap.empty,

    // Numerical assertions
    // https://json-schema.org/draft/2020-12/json-schema-validation#section-6.2
    multipleOf: Option[BigDecimal] = None,
    minimum: Option[BigDecimal] = None,
    exclusiveMinimum: Option[BigDecimal] = None,
    maximum: Option[BigDecimal] = None,
    exclusiveMaximum: Option[BigDecimal] = None,

    // String assertions
    // https://json-schema.org/draft/2020-12/json-schema-validation#section-6.3
    maxLength: Option[Int] = None,
    minLength: Option[Int] = None,
    pattern: Option[Pattern] = None,

    // Array assertions
    // https://json-schema.org/draft/2020-12/json-schema-validation#section-6.4
    maxItems: Option[Int] = None,
    minItems: Option[Int] = None,
    uniqueItems: Option[Boolean] = None,
    maxContains: Option[Int] = None,
    minContains: Option[Int] = None,

    // Array applicators
    // https://json-schema.org/draft/2020-12/json-schema-core#section-10.3.1
    prefixItems: Option[List[SchemaLike]] = None,
    items: Option[SchemaLike] = None,
    contains: Option[SchemaLike] = None,
    // https://json-schema.org/draft/2020-12/json-schema-core#section-11.2
    unevaluatedItems: Option[SchemaLike] = None,

    // Object assertions
    // https://json-schema.org/draft/2020-12/json-schema-validation#section-6.5
    maxProperties: Option[Int] = None,
    minProperties: Option[Int] = None,
    required: List[String] = List.empty,
    dependentRequired: ListMap[String, List[String]] = ListMap.empty,
    // OpenAPI specific
    // https://spec.openapis.org/oas/v3.1.0#fixed-fields-19
    discriminator: Option[Discriminator] = None,

    // Object applicators
    // https://json-schema.org/draft/2020-12/json-schema-core#section-10.3.2
    properties: ListMap[String, SchemaLike] = ListMap.empty,
    patternProperties: ListMap[Pattern, SchemaLike] = ListMap.empty,
    additionalProperties: Option[SchemaLike] = None,
    propertyNames: Option[SchemaLike] = None,
    // https://json-schema.org/draft/2020-12/json-schema-core#section-11.3
    unevaluatedProperties: Option[SchemaLike] = None,

    // OpenAPI specific
    // https://spec.openapis.org/oas/v3.1.0#fixed-fields-19
    externalDocs: Option[ExternalDocumentation] = None,
    // https://spec.openapis.org/oas/v3.1.0#specification-extensions
    extensions: ListMap[String, ExtensionValue] = ListMap.empty
) extends SchemaLike {

  /** Returns a Schema that allows `null` values in addition to the current schema. The implementation is idempotent,
    * i.e. `schema.nullable.nullable == schema.nullable`.
    */
  def nullable: Schema = `type` match {
    case Some(types) =>
      if (types.contains(SchemaType.Null)) this // ensure idempotency
      else copy(`type` = Some(types :+ SchemaType.Null))

    case None =>
      val nullSchema = Schema(SchemaType.Null)
      if(anyOf.contains(nullSchema)) this // ensure idempotency
      else if (anyOf.nonEmpty) copy(anyOf = anyOf :+ nullSchema)
      else Schema(anyOf = List(this, nullSchema))
  }
}

case class Discriminator(propertyName: String, mapping: Option[ListMap[String, String]])

object Schema {
  def apply(schemaType: SchemaType, moreTypes: SchemaType*): Schema =
    new Schema(`type` = Some(schemaType :: moreTypes.toList))

  def oneOf(references: List[SchemaLike], discriminator: Option[Discriminator]): Schema =
    Schema(oneOf = references, discriminator = discriminator)

  def referenceTo(prefix: String, $ref: String): Schema =
    Schema($ref = Some(s"$prefix${$ref}"))
}

sealed abstract class SchemaType(val value: String)
object SchemaType {
  case object Boolean extends SchemaType("boolean")
  case object Object extends SchemaType("object")
  case object Array extends SchemaType("array")
  case object Number extends SchemaType("number")
  case object String extends SchemaType("string")
  case object Integer extends SchemaType("integer")
  case object Null extends SchemaType("null")
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
