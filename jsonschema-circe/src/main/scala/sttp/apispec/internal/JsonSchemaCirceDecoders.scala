package sttp.apispec
package internal

import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto.deriveDecoder
import scala.collection.immutable.ListMap

trait JsonSchemaCirceDecoders {
  implicit val referenceDecoder: Decoder[Reference] = deriveDecoder[Reference]
  implicit def decodeReferenceOr[A: Decoder]: Decoder[ReferenceOr[A]] = referenceDecoder.either(Decoder[A])

  implicit val decodeBasicSchemaType: Decoder[BasicSchemaType] = Decoder.decodeString.emap {
    case SchemaType.Integer.value => SchemaType.Integer.asRight
    case SchemaType.Boolean.value => SchemaType.Boolean.asRight
    case SchemaType.String.value => SchemaType.String.asRight
    case SchemaType.Number.value => SchemaType.Number.asRight
    case SchemaType.Array.value => SchemaType.Array.asRight
    case SchemaType.Object.value => SchemaType.Object.asRight
    case SchemaType.Null.value => SchemaType.Null.asRight
    case err => s"$err is an unknown schema type".asLeft
  }

  implicit val decodeArraySchemaType: Decoder[ArraySchemaType] = {
    Decoder.decodeList(decodeBasicSchemaType).map(ArraySchemaType.apply)
  }

  implicit val decodePatternKey: KeyDecoder[Pattern] =
    KeyDecoder.decodeKeyString.map(Pattern.apply)

  implicit val decodePattern: Decoder[Pattern] =
    Decoder.decodeString.map(Pattern.apply)

  implicit val decodeSchemaType: Decoder[SchemaType] =
    decodeBasicSchemaType.widen[SchemaType].or(decodeArraySchemaType.widen[SchemaType])

  implicit val exampleSingleValueDecoder: Decoder[ExampleSingleValue] =
    Decoder[Json].map(json => json.asString.map(ExampleSingleValue(_)).getOrElse(ExampleSingleValue(json)))

  implicit val exampleMultipleValueDecoder: Decoder[ExampleMultipleValue] =
    Decoder[List[Json]].map { json =>
      val listString = json.flatMap(_.asString)
      if (listString.nonEmpty) {
        ExampleMultipleValue(listString)
      } else ExampleMultipleValue(json)
    }

  implicit val discriminatorDecoder: Decoder[Discriminator] = deriveDecoder[Discriminator]


  implicit val exampleValueDecoder: Decoder[ExampleValue] =
    exampleMultipleValueDecoder.widen[ExampleValue].or(exampleSingleValueDecoder.widen[ExampleValue])

  implicit val extensionValueDecoder: Decoder[ExtensionValue] = Decoder[Json].map(j => ExtensionValue(j.spaces2))

  implicit val extensionsDecoder: Decoder[ListMap[String, ExtensionValue]] =
    Decoder.decodeMapLike[String, ExtensionValue, ListMap].map(_.filter(_._1.startsWith("x-")))


  implicit val schemaDecoder: Decoder[Schema] = {
    implicit def listMapDecoder[A: Decoder]: Decoder[ListMap[String, ReferenceOr[A]]] =
      Decoder.decodeOption(Decoder.decodeMapLike[String, ReferenceOr[A], ListMap]).map(_.getOrElse(ListMap.empty))

    implicit def listPatternMapDecoder[A: Decoder]: Decoder[ListMap[Pattern, ReferenceOr[A]]] =
      Decoder.decodeOption(Decoder.decodeMapLike[Pattern, ReferenceOr[A], ListMap]).map(_.getOrElse(ListMap.empty))

    implicit def listReference[A: Decoder]: Decoder[List[A]] =
      Decoder.decodeOption(Decoder.decodeList[A]).map(_.getOrElse(Nil))

    def translateMinMax[A](decoder: Decoder[A]) = Decoder.instance { c =>
      val modded = c.withFocus(_.mapObject { obj =>
        val map = obj.toMap
        val min = map
          .get("exclusiveMinimum")
          .map { m =>
            if (m.isNumber) obj.remove("exclusiveMinimum").add("exclusiveMinimum", Json.True).add("minimum", m) else obj
          }
          .getOrElse(obj)
        map
          .get("exclusiveMaximum")
          .map { m =>
            if (m.isNumber) min.remove("exclusiveMaximum").add("exclusiveMaximum", Json.True).add("maximum", m) else min
          }
          .getOrElse(min)
      })
      decoder.tryDecode(modded)
    }

    withExtensions(
      translateMinMax(
        deriveDecoder[Schema].map(s =>
          s.`type` match {
            case Some(ArraySchemaType(x :: SchemaType.Null :: Nil)) => s.copy(`type` = Some(x), nullable = Some(true))
            case _ => s
          }
        )
      )
    )
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
