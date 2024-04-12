package sttp.apispec
package internal

import cats.syntax.all._
import io.circe._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.syntax._

import scala.collection.immutable.ListMap

trait JsonSchemaCirceDecoders {
  implicit val decodeSchemaType: Decoder[SchemaType] = Decoder.decodeString.emap {
    case SchemaType.Integer.value => SchemaType.Integer.asRight
    case SchemaType.Boolean.value => SchemaType.Boolean.asRight
    case SchemaType.String.value  => SchemaType.String.asRight
    case SchemaType.Number.value  => SchemaType.Number.asRight
    case SchemaType.Array.value   => SchemaType.Array.asRight
    case SchemaType.Object.value  => SchemaType.Object.asRight
    case SchemaType.Null.value    => SchemaType.Null.asRight
    case err                      => s"$err is an unknown schema type".asLeft
  }

  implicit val decodePatternKey: KeyDecoder[Pattern] =
    KeyDecoder.decodeKeyString.map(Pattern.apply)

  implicit val decodePattern: Decoder[Pattern] =
    Decoder.decodeString.map(Pattern.apply)

  implicit val discriminatorDecoder: Decoder[Discriminator] = deriveDecoder[Discriminator]

  implicit val exampleValueDecoder: Decoder[ExampleValue] =
    Decoder[Json].map(json => json.asString.map(ExampleValue(_)).getOrElse(ExampleValue(json)))

  implicit val extensionValueDecoder: Decoder[ExtensionValue] = Decoder[Json].map(j => ExtensionValue(j.spaces2))

  implicit val extensionsDecoder: Decoder[ListMap[String, ExtensionValue]] =
    Decoder.decodeMapLike[String, ExtensionValue, ListMap].map(_.filter(_._1.startsWith("x-")))

  // OpenAPI extension to JSON Schema
  implicit val externalDocumentationDecoder: Decoder[ExternalDocumentation] =
    withExtensions(deriveDecoder[ExternalDocumentation])

  implicit val schemaDecoder: Decoder[Schema] = {
    implicit def listMapDecoder[A: Decoder]: Decoder[ListMap[String, A]] =
      Decoder.decodeOption(Decoder.decodeMapLike[String, A, ListMap]).map(_.getOrElse(ListMap.empty))

    implicit def listPatternMapDecoder[A: Decoder]: Decoder[ListMap[Pattern, A]] =
      Decoder.decodeOption(Decoder.decodeMapLike[Pattern, A, ListMap]).map(_.getOrElse(ListMap.empty))

    implicit def listDependentFieldsDecoder: Decoder[ListMap[String, List[String]]] =
      Decoder.decodeOption(Decoder.decodeMapLike[String, List[String], ListMap]).map(_.getOrElse(ListMap.empty))

    implicit def listReference[A: Decoder]: Decoder[List[A]] =
      Decoder.decodeOption(Decoder.decodeList[A]).map(_.getOrElse(Nil))

    def translateDefinitionsTo$def(obj: JsonObject): JsonObject = {
      val map = obj.toMap
      val definitions = map.get("definitions").orElse(map.get("$defs"))
      definitions.map(j => obj.remove("definitions").remove("$defs").add("$defs", j)).getOrElse(obj)
    }

    // OAS 3.0: { "maximum": 10, "exclusiveMaximum": true }
    // OAS 3.1: { "exclusiveMaximum": 10 }
    def adjustMaximum(obj: JsonObject): JsonObject =
      (obj("maximum"), obj("exclusiveMaximum")) match {
        case (Some(max), Some(Json.True)) => obj.remove("maximum").add("exclusiveMaximum", max)
        case _ => obj
      }

    // OAS 3.0: { "minimum": 10, "exclusiveMinimum": true }
    // OAS 3.1: { "exclusiveMinimum": 10 }
    def adjustMinimum(obj: JsonObject): JsonObject =
      (obj("minimum"), obj("exclusiveMinimum")) match {
        case (Some(min), Some(Json.True)) => obj.remove("minimum").add("exclusiveMinimum", min)
        case _ => obj
      }

    // OAS 3.0: { "example": "exampleValue" }
    // OAS 3.1: { "examples": ["exampleValue"] }
    def adjustExample(obj: JsonObject): JsonObject =
      obj("example") match {
        case Some(example) => obj.remove("example").add("examples", Json.arr(example))
        case _ => obj
      }

    // Both OAS 3.0 and OAS 3.1 allow `type` to be a string or an array of strings,
    // but we model it as a List in the schema case class, and so the derived decoder expects an array.
    def adjustType(obj: JsonObject): JsonObject =
      obj("type") match {
        case Some(tpe) if tpe.isString => obj.add("type", Json.arr(tpe))
        case _ => obj
      }

    def adjustSyntax(decoder: Decoder[Schema]) = Decoder.instance { c =>
      val nullable = c.get[Boolean]("nullable").contains(true)
      val modded = c.withFocus(_
        .mapObject(adjustType)
        .mapObject(adjustMaximum)
        .mapObject(adjustMinimum)
        .mapObject(adjustExample)
        .mapObject(translateDefinitionsTo$def)
      )
      decoder.tryDecode(modded).map(s => if(nullable) s.nullable else s)
    }

    adjustSyntax(withExtensions(deriveDecoder[Schema]))
  }

  implicit val anySchemaDecoder: Decoder[AnySchema] = Decoder.instance { c =>
    def fromBool(b: Boolean) =
      if (b) AnySchema.Anything else AnySchema.Nothing

    def fromObject(obj: JsonObject) = {
      val target = JsonObject("not" := Json.obj())

      if (obj.isEmpty) {
        AnySchema.Anything.some
      } else if (obj == target) {
        AnySchema.Nothing.some
      } else
        none[AnySchema]
    }

    c.focus
      .flatMap(
        _.fold(
          none[AnySchema],
          fromBool(_).some,
          _ => none[AnySchema],
          _ => none[AnySchema],
          _ => none[AnySchema],
          fromObject
        )
      )
      .toRight(DecodingFailure("Unable to decode AnyObject", c.history))
  }
  implicit val schemaLikeDecoder: Decoder[SchemaLike] =
    anySchemaDecoder.widen[SchemaLike].or(schemaDecoder.widen[SchemaLike])

  def withExtensions[A](decoder: Decoder[A]): Decoder[A] = Decoder.instance { c =>
    val modded = c.withFocus(json =>
      json
        .mapObject { obj =>
          val withoutExt = obj.filterKeys(!_.startsWith("x-"))
          withoutExt.add("extensions", obj.filterKeys(_.startsWith("x-")).asJson)
        }
    )
    decoder.tryDecode(modded)
  }
}
