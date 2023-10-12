package sttp.apispec
package internal

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.parse

import scala.collection.immutable.ListMap

trait JsonSchemaCirceEncoders {
  def anyObjectEncoding: AnySchema.Encoding

  def openApi30: Boolean = false

  val jsonSchemaEncoder: Encoder[Schema] = Encoder.AsObject
    .instance { (s: Schema) =>
      val minKey = if (s.exclusiveMinimum.getOrElse(false)) "exclusiveMinimum" else "minimum"
      val maxKey = if (s.exclusiveMaximum.getOrElse(false)) "exclusiveMaximum" else "maximum"
      JsonObject(
        "$id" := s.$id,
        "$ref" := s.$ref,
        "$schema" := s.$schema,
        "allOf" := s.allOf,
        "anyOf" := s.anyOf,
        "title" := s.title,
        "required" := s.required,
        "type" := (if (s.nullable.getOrElse(false))
                     s.`type`.map(s => Json.arr(s.asJson, Json.fromString("null"))).asJson
                   else s.`type`.asJson),
        "prefixItems" := s.prefixItems,
        "items" := s.items,
        "contains" := s.contains,
        "properties" := s.properties,
        "patternProperties" := s.patternProperties,
        "description" := s.description,
        "format" := s.format,
        "default" := s.default,
        "readOnly" := s.readOnly,
        "writeOnly" := s.writeOnly,
        "example" := s.example,
        "deprecated" := s.deprecated,
        "oneOf" := s.oneOf,
        "discriminator" := s.discriminator,
        "additionalProperties" := s.additionalProperties,
        "pattern" := s.pattern,
        "minLength" := s.minLength,
        "maxLength" := s.maxLength,
        minKey := s.minimum,
        maxKey := s.maximum,
        "minItems" := s.minItems,
        "maxItems" := s.maxItems,
        "enum" := s.`enum`,
        "not" := s.not,
        "if" := s.`if`,
        "then" := s.`then`,
        "else" := s.`else`,
        "$defs" := s.$defs,
        "const" := s.const,
        "unevaluatedProperties" := s.unevaluatedProperties,
        "dependentRequired" := s.dependentRequired,
        "dependentSchemas" := s.dependentSchemas,
        "extensions" := s.extensions
      )
    }
    .mapJsonObject(expandExtensions)

  val encoderSchema30: Encoder[Schema] = Encoder.AsObject
    .instance { (s: Schema) =>
      JsonObject(
        "$ref" := s.$ref,
        "allOf" := s.allOf,
        "title" := s.title,
        "required" := s.required,
        "type" := s.`type`,
        "prefixItems" := s.prefixItems,
        "items" := s.items,
        "contains" := s.contains,
        "properties" := s.properties,
        "patternProperties" := s.patternProperties,
        "description" := s.description,
        "format" := s.format,
        "default" := s.default,
        "readOnly" := s.readOnly,
        "writeOnly" := s.writeOnly,
        // the current Schema model currently supports a single, optional example; if multiple examples support is added, they should be serialised to "examples"
        "example" := s.example,
        "deprecated" := s.deprecated,
        "oneOf" := s.oneOf,
        "discriminator" := s.discriminator,
        "additionalProperties" := s.additionalProperties,
        "pattern" := s.pattern,
        "minLength" := s.minLength,
        "maxLength" := s.maxLength,
        "minimum" := s.minimum,
        "exclusiveMinimum" := s.exclusiveMinimum,
        "maximum" := s.maximum,
        "exclusiveMaximum" := s.exclusiveMaximum,
        "minItems" := s.minItems,
        "maxItems" := s.maxItems,
        "enum" := s.`enum`,
        "not" := s.not,
        "nullable" := s.nullable,
        "extensions" := s.extensions
      )
    }
    .mapJsonObject(expandExtensions)

  // note: these are strict val-s, order matters!
  implicit val extensionValue: Encoder[ExtensionValue] =
    Encoder.instance(e => parse(e.value).getOrElse(Json.fromString(e.value)))

  implicit val encoderExampleSingleValue: Encoder[ExampleSingleValue] = {
    case ExampleSingleValue(value: String)     => parse(value).getOrElse(Json.fromString(value))
    case ExampleSingleValue(value: Int)        => Json.fromInt(value)
    case ExampleSingleValue(value: Long)       => Json.fromLong(value)
    case ExampleSingleValue(value: Float)      => Json.fromFloatOrString(value)
    case ExampleSingleValue(value: Double)     => Json.fromDoubleOrString(value)
    case ExampleSingleValue(value: Boolean)    => Json.fromBoolean(value)
    case ExampleSingleValue(value: BigDecimal) => Json.fromBigDecimal(value)
    case ExampleSingleValue(value: BigInt)     => Json.fromBigInt(value)
    case ExampleSingleValue(null)              => Json.Null
    case ExampleSingleValue(value)             => Json.fromString(value.toString)
  }

  implicit val encoderMultipleExampleValue: Encoder[ExampleMultipleValue] = { e =>
    Json.arr(e.values.map(v => encoderExampleSingleValue(ExampleSingleValue(v))): _*)
  }

  implicit val encoderExampleValue: Encoder[ExampleValue] = {
    case e: ExampleMultipleValue => encoderMultipleExampleValue.apply(e)
    case e: ExampleSingleValue   => encoderExampleSingleValue.apply(e)
  }

  implicit val encoderSchemaType: Encoder[SchemaType] = {
    case e: BasicSchemaType   => e.value.asJson
    case ArraySchemaType(typ) => typ.map(_.value.asJson).asJson
  }
  implicit val encoderKeyPattern: KeyEncoder[Pattern] =
    KeyEncoder.encodeKeyString.contramap(_.value)
  implicit val encoderPattern: Encoder[Pattern] =
    Encoder.encodeString.contramap(_.value)

  implicit val encoderDiscriminator: Encoder[Discriminator] = deriveEncoder[Discriminator]

  implicit lazy val encoderSchema: Encoder[Schema] = if (openApi30) encoderSchema30 else jsonSchemaEncoder

  implicit val encoderAnySchema: Encoder[AnySchema] = Encoder.instance {
    case AnySchema.Anything =>
      anyObjectEncoding match {
        case AnySchema.Encoding.Object  => Json.obj()
        case AnySchema.Encoding.Boolean => Json.True
      }
    case AnySchema.Nothing =>
      anyObjectEncoding match {
        case AnySchema.Encoding.Object =>
          Json.obj(
            "not" := Json.obj()
          )
        case AnySchema.Encoding.Boolean => Json.False
      }
  }

  implicit val encoderSchemaLike: Encoder[SchemaLike] = Encoder.instance {
    case s: AnySchema => encoderAnySchema(s)
    case s: Schema    => encoderSchema(s)
  }

  implicit def encodeList[T: Encoder]: Encoder[List[T]] = {
    case Nil        => Json.Null
    case l: List[T] => Json.arr(l.map(i => implicitly[Encoder[T]].apply(i)): _*)
  }

  implicit def encodeListMap[K: KeyEncoder, V: Encoder]: Encoder[ListMap[K, V]] = doEncodeListMap(nullWhenEmpty = true)

  private[apispec] def doEncodeListMap[K: KeyEncoder, V: Encoder](nullWhenEmpty: Boolean): Encoder[ListMap[K, V]] = {
    case m: ListMap[K, V] if m.isEmpty && nullWhenEmpty => Json.Null
    case m: ListMap[K, V] =>
      val properties = m.map { case (k, v) => KeyEncoder[K].apply(k) -> Encoder[V].apply(v) }.toList
      Json.obj(properties: _*)
  }

  /*
      Openapi extensions are arbitrary key-value data that could be added to some of models in specifications, such
      as `OpenAPI` itself, `License`, `Parameter`, etc.

      The key could be any string (that starts with 'x-' by convention) and value is arbitrary Json (string, object,
      array, etc.)

      To be able to encode such arbitrary data and apply it to the final Json it passed through the `extensions` field
      in models and moved (or expanded) to the object level while encoding

      Example:

      ```
      case class License(
         name: String,
         url: Option[String],
         extensions: ListMap[String, ExtensionValue] = ListMap.empty
      )

      val licenseWithExtension = License("hello", None, ListMap("x-foo", ExtensionValue("42"))
      ```

      Applying the transformation below we end up with the following schema in the specification:

      ```
      license:
        name: hello
        x-foo: 42
      ```
   */
  private[apispec] def expandExtensions(jsonObject: JsonObject): JsonObject = {
    val jsonWithoutExt = jsonObject.filterKeys(_ != "extensions")
    jsonObject("extensions")
      .flatMap(_.asObject)
      .map { extObject =>
        val allKeys = (jsonWithoutExt.keys ++ extObject.keys).toSeq.distinct
        allKeys.foldLeft(JsonObject.empty) { case (acc, key) =>
          extObject(key).orElse(jsonWithoutExt(key)) match {
            case Some(value) => acc.add(key, value)
            case None        => acc
          }
        }
      }
      .getOrElse(jsonWithoutExt)
  }

}
