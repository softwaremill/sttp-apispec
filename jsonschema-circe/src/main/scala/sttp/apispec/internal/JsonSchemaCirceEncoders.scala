package sttp.apispec
package internal

import io.circe._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.parse
import io.circe.syntax._
import sttp.apispec.internal.JsonSchemaCirceEncoders.ObjectEncoderOps

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

trait JsonSchemaCirceEncoders {
  def anyObjectEncoding: AnySchema.Encoding

  def openApi30: Boolean = false

  protected final implicit def objectEncoderOps[T](encoder: Encoder.AsObject[T]): ObjectEncoderOps[T] =
    new ObjectEncoderOps(encoder)

  implicit lazy val encoderSchema: Encoder[Schema] = Encoder.AsObject.instance { (s: Schema) =>
    val nullSchema = Schema(`type` = Some(List(SchemaType.Null)))

    // In OpenAPI 3.0 the keyword "const" is not allowed and needs to be replaced by an "enum" with one element
    val enumAndConstFields =
      if (openApi30 && s.const.isDefined)
        Vector("enum" := s.const.map(List(_)), "const" := None)
      else
        Vector("enum" := s.`enum`, "const" := s.const)

    // Nullable $ref Schema is represented as {"anyOf": [{"$ref": "some-ref"}, {"type": "null"}]}
    // In OpenAPI 3.0, we need to translate it to {"allOf": [{"$ref": "some-ref"}], "nullable": true}
    val wrappedNullableRef30 = s.anyOf match {
      case List(refSchema: Schema, `nullSchema`) if refSchema.$ref.isDefined && openApi30 =>
        Some(refSchema)
      case _ => None
    }

    val typeAndNullable = s.`type` match {
      case Some(List(tpe)) =>
        Vector("type" := tpe)
      case Some(List(tpe, SchemaType.Null)) if openApi30 =>
        Vector("type" := tpe, "nullable" := true)
      case None if wrappedNullableRef30.isDefined =>
        Vector("nullable" := true)
      case t =>
        Vector("type" := t)
    }

    val minFields = (s.minimum, s.exclusiveMinimum) match {
      case (None, Some(min)) if openApi30 =>
        Vector("minimum" := min, "exclusiveMinimum" := true)
      case _ =>
        Vector("minimum" := s.minimum, "exclusiveMinimum" := s.exclusiveMinimum)
    }

    val maxFields = (s.maximum, s.exclusiveMaximum) match {
      case (None, Some(max)) if openApi30 =>
        Vector("maximum" := max, "exclusiveMaximum" := true)
      case _ =>
        Vector("maximum" := s.maximum, "exclusiveMaximum" := s.exclusiveMaximum)
    }

    val exampleFields = s.examples match {
      case Some(List(example)) if openApi30 =>
        Vector("example" := example)
      case _ =>
        Vector("examples" := s.examples)
    }

    JsonObject.fromIterable(
      Vector(
        "$schema" := s.$schema,
        "$vocabulary" := s.$vocabulary,
        "$id" := s.$id,
        "$anchor" := s.$anchor,
        "$dynamicAnchor" := s.$dynamicAnchor,
        "$ref" := s.$ref,
        "$dynamicRef" := s.$dynamicRef,
        "$comment" := s.$comment,
        "$defs" := s.$defs,
        "title" := s.title,
        "description" := s.description,
        "default" := s.default,
        "deprecated" := s.deprecated,
        "readOnly" := s.readOnly,
        "writeOnly" := s.writeOnly
      ) ++ exampleFields ++ typeAndNullable ++ enumAndConstFields ++ Vector(
        "format" := s.format,
        "allOf" := wrappedNullableRef30.map(List(_)).getOrElse(s.allOf),
        "anyOf" := (if (wrappedNullableRef30.isDefined) Nil else s.anyOf),
        "oneOf" := s.oneOf,
        "not" := s.not,
        "if" := s.`if`,
        "then" := s.`then`,
        "else" := s.`else`,
        "dependentSchemas" := s.dependentSchemas,
        "multipleOf" := s.multipleOf
      ) ++ minFields ++ maxFields ++ Vector(
        "maxLength" := s.maxLength,
        "minLength" := s.minLength,
        "pattern" := s.pattern,
        "maxItems" := s.maxItems,
        "minItems" := s.minItems,
        "uniqueItems" := s.uniqueItems,
        "maxContains" := s.maxContains,
        "minContains" := s.minContains,
        "prefixItems" := s.prefixItems,
        "items" := s.items,
        "contains" := s.contains,
        "unevaluatedItems" := s.unevaluatedItems,
        "maxProperties" := s.maxProperties,
        "minProperties" := s.minProperties,
        "required" := s.required,
        "dependentRequired" := s.dependentRequired,
        "discriminator" := s.discriminator,
        "properties" := s.properties,
        "patternProperties" := s.patternProperties,
        "additionalProperties" := s.additionalProperties,
        "propertyNames" := s.propertyNames,
        "unevaluatedProperties" := s.unevaluatedProperties,
        "externalDocs" := s.externalDocs,
        "extensions" := s.extensions
      )
    )
  }.dropNullsExpandExtensions

  // note: these are strict val-s, order matters!
  implicit val extensionValue: Encoder[ExtensionValue] =
    Encoder.instance(e => parse(e.value).getOrElse(Json.fromString(e.value)))

  implicit val encoderExampleSingleValue: Encoder[ExampleSingleValue] = {
    case ExampleSingleValue(value: String)     => parse(value).getOrElse(Json.fromString(value))
    case ExampleSingleValue(value: Int)        => Json.fromInt(value)
    case ExampleSingleValue(value: Short)      => Json.fromInt(value)
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

  implicit val encoderSchemaType: Encoder[SchemaType] =
    _.value.asJson

  implicit val encoderKeyPattern: KeyEncoder[Pattern] =
    KeyEncoder.encodeKeyString.contramap(_.value)

  implicit val encoderPattern: Encoder[Pattern] =
    Encoder.encodeString.contramap(_.value)

  implicit val encoderDiscriminator: Encoder[Discriminator] =
    deriveEncoder[Discriminator].dropNulls

  implicit val encoderExternalDocumentation: Encoder[ExternalDocumentation] =
    deriveEncoder[ExternalDocumentation].dropNullsExpandExtensions

  implicit val encoderAnySchema: Encoder[AnySchema] = Encoder.instance {
    case AnySchema.Anything =>
      anyObjectEncoding match {
        case AnySchema.Encoding.Object  => Json.obj()
        case AnySchema.Encoding.Boolean => Json.True
      }
    case AnySchema.Nothing =>
      anyObjectEncoding match {
        case AnySchema.Encoding.Object =>
          Json.obj("not" := Json.obj())
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

  // just for backward compatibility
  private[apispec] def expandExtensions(jsonObject: JsonObject): JsonObject =
    JsonSchemaCirceEncoders.expandExtensions(jsonObject)

}
object JsonSchemaCirceEncoders {
  class ObjectEncoderOps[T](private val encoder: Encoder.AsObject[T]) extends AnyVal {
    def dropNulls: Encoder.AsObject[T] =
      encoder.mapJsonObject(_.filter { case (_, v) => !v.isNull })

    def dropNullsExpandExtensions: Encoder.AsObject[T] =
      dropNulls.mapJsonObject(expandExtensions)
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
  private def expandExtensions(jsonObject: JsonObject): JsonObject = {
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
