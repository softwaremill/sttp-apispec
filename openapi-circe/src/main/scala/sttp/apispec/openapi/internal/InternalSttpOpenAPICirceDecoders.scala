package sttp.apispec
package openapi
package internal

import cats.syntax.all._
import io.circe.{Decoder, KeyDecoder, DecodingFailure, Json, JsonObject}
import io.circe.syntax._
import io.circe.generic.semiauto.deriveDecoder
import sttp.apispec.{Reference, ReferenceOr, Schema, SchemaType}

import scala.collection.immutable.ListMap

trait InternalSttpOpenAPICirceDecoders {
  implicit val referenceDecoder: Decoder[Reference] = deriveDecoder[Reference]
  implicit def decodeReferenceOr[A: Decoder]: Decoder[ReferenceOr[A]] = referenceDecoder.either(Decoder[A])

  implicit val decodeBasicSchemaType: Decoder[BasicSchemaType] = Decoder.decodeString.emap {
    case SchemaType.Integer.value => SchemaType.Integer.asRight
    case SchemaType.Boolean.value => SchemaType.Boolean.asRight
    case SchemaType.String.value  => SchemaType.String.asRight
    case SchemaType.Number.value  => SchemaType.Number.asRight
    case SchemaType.Array.value   => SchemaType.Array.asRight
    case SchemaType.Object.value  => SchemaType.Object.asRight
    case SchemaType.Null.value    => SchemaType.Null.asRight
    case err                      => s"$err is an unknown schema type".asLeft
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

  implicit val exampleValueDecoder: Decoder[ExampleValue] =
    exampleMultipleValueDecoder.widen[ExampleValue].or(exampleSingleValueDecoder.widen[ExampleValue])

  implicit val extensionValueDecoder: Decoder[ExtensionValue] = Decoder[Json].map(j => ExtensionValue(j.spaces2))

  implicit val extensionsDecoder: Decoder[ListMap[String, ExtensionValue]] =
    Decoder.decodeMapLike[String, ExtensionValue, ListMap].map(_.filter(_._1.startsWith("x-")))

  implicit val discriminatorDecoder: Decoder[Discriminator] = deriveDecoder[Discriminator]

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
            obj.remove("exclusiveMinimum").add("exclusiveMinimum", Json.True).add("minimum", m)
          }
          .getOrElse(obj)
        map
          .get("exclusiveMaximum")
          .map { m =>
            min.remove("exclusiveMaximum").add("exclusiveMaximum", Json.True).add("maximum", m)
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
            case _                                                  => s
          }
        )
      )
    )
  }
  implicit val anySchemaDecoder: Decoder[AnySchema] = Decoder.instance { c =>
    def fromBool(b: Boolean) =
      if (b) AnySchema.Anything(AnySchema.Encoding.Boolean) else AnySchema.Nothing(AnySchema.Encoding.Boolean)

    def fromObject(obj: JsonObject) = {
      val target = JsonObject("not" := Json.obj())

      if (obj.isEmpty) {
        AnySchema.Anything(AnySchema.Encoding.Object).some
      } else if (obj == target) {
        AnySchema.Nothing(AnySchema.Encoding.Object).some
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

  implicit val externalDocumentationDecoder: Decoder[ExternalDocumentation] = withExtensions(
    deriveDecoder[ExternalDocumentation]
  )
  implicit val tagDecoder: Decoder[Tag] = withExtensions(deriveDecoder[Tag])

  implicit val oauthFlowDecoder: Decoder[OAuthFlow] = withExtensions(deriveDecoder[OAuthFlow])
  implicit val oauthFlowsDecoder: Decoder[OAuthFlows] = withExtensions(deriveDecoder[OAuthFlows])
  implicit val securitySchemeDecoder: Decoder[SecurityScheme] = withExtensions(deriveDecoder[SecurityScheme])

  implicit val contactDecoder: Decoder[Contact] = withExtensions(deriveDecoder[Contact])
  implicit val licenseDecoder: Decoder[License] = withExtensions(deriveDecoder[License])
  implicit val infoDecoder: Decoder[Info] = withExtensions(deriveDecoder[Info])

  implicit val serverVariableDecoder: Decoder[ServerVariable] = deriveDecoder[ServerVariable]
  implicit val serverDecoder: Decoder[Server] = deriveDecoder[Server]
  implicit val linkDecoder: Decoder[Link] = deriveDecoder[Link]

  implicit val parameterInDecoder: Decoder[ParameterIn] = Decoder.decodeString.emap {
    case ParameterIn.Path.value   => ParameterIn.Path.asRight
    case ParameterIn.Query.value  => ParameterIn.Query.asRight
    case ParameterIn.Cookie.value => ParameterIn.Cookie.asRight
    case ParameterIn.Header.value => ParameterIn.Header.asRight
    case err                      => s"$err is not a valid ParameterIn value".asLeft
  }
  implicit val parameterStyleDecoder: Decoder[ParameterStyle] = Decoder.decodeString.emap {
    case ParameterStyle.Form.value           => ParameterStyle.Form.asRight
    case ParameterStyle.Label.value          => ParameterStyle.Label.asRight
    case ParameterStyle.Matrix.value         => ParameterStyle.Matrix.asRight
    case ParameterStyle.Simple.value         => ParameterStyle.Simple.asRight
    case ParameterStyle.SpaceDelimited.value => ParameterStyle.SpaceDelimited.asRight
    case ParameterStyle.DeepObject.value     => ParameterStyle.DeepObject.asRight
    case ParameterStyle.PipeDelimited.value  => ParameterStyle.PipeDelimited.asRight
    case err                                 => s"$err is not a valid ParameterStyle value".asLeft
  }

  implicit val exampleDecoder: Decoder[Example] = withExtensions(deriveDecoder[Example])
  implicit val encodingDecoder: Decoder[Encoding] = withExtensions(deriveDecoder[Encoding])
  implicit val headerDecoder: Decoder[Header] = deriveDecoder[Header]
  implicit val mediaTypeDecoder: Decoder[MediaType] = withExtensions(deriveDecoder[MediaType])
  implicit val requestBodyDecoder: Decoder[RequestBody] = withExtensions(deriveDecoder[RequestBody])

  implicit val responseDecoder: Decoder[Response] = {
    implicit def listMapDecoder[A: Decoder]: Decoder[ListMap[String, ReferenceOr[A]]] =
      Decoder.decodeOption(Decoder.decodeMapLike[String, ReferenceOr[A], ListMap]).map(_.getOrElse(ListMap.empty))

    implicit def listMapMediaTypeDecoder: Decoder[ListMap[String, MediaType]] =
      Decoder.decodeOption(Decoder.decodeMapLike[String, MediaType, ListMap]).map(_.getOrElse(ListMap.empty))

    withExtensions(deriveDecoder[Response])
  }
  implicit val responsesKeyDecoder: KeyDecoder[ResponsesKey] = {
    val Range = "(\\d)XX".r
    KeyDecoder.decodeKeyString.map {
      case "default"    => ResponsesDefaultKey
      case Range(range) => ResponsesRangeKey(range.toInt)
      case code         => ResponsesCodeKey(code.toInt)
    }
  }

  implicit val responsesDecoder: Decoder[Responses] = withExtensions(Decoder.instance { c =>
    for {
      responses <- c
        .as[JsonObject]
        .flatMap(json => json.remove("extensions").asJson.as[ListMap[ResponsesKey, ReferenceOr[Response]]])
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Responses(responses, extensions)
  })
  implicit val parameterDecoder: Decoder[Parameter] = withExtensions(deriveDecoder[Parameter])
  implicit val callbackDecoder: Decoder[Callback] = deriveDecoder[Callback]
  implicit val operationDecoder: Decoder[Operation] = {
    implicit def listMapDecoder[A: Decoder]: Decoder[ListMap[String, ReferenceOr[A]]] =
      Decoder.decodeOption(Decoder.decodeMapLike[String, ReferenceOr[A], ListMap]).map(_.getOrElse(ListMap.empty))

    implicit def listReference[A: Decoder]: Decoder[List[A]] =
      Decoder.decodeOption(Decoder.decodeList[A]).map(_.getOrElse(Nil))

    withExtensions(deriveDecoder[Operation])
  }
  implicit val pathItemDecoder: Decoder[PathItem] = {
    implicit def listReference[A: Decoder]: Decoder[List[A]] =
      Decoder.decodeOption(Decoder.decodeList[A]).map(_.getOrElse(Nil))

    withExtensions(deriveDecoder[PathItem])
  }
  implicit val pathsDecoder: Decoder[Paths] = withExtensions(Decoder.instance { c =>
    for {
      pathItems <- c.as[JsonObject].flatMap(json => json.remove("extensions").asJson.as[ListMap[String, PathItem]])
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Paths(pathItems, extensions)
  })
  implicit val componentsDecoder: Decoder[Components] = withExtensions(Decoder.instance { c =>
    type Comp[A] = ListMap[String, ReferenceOr[A]]
    def getComp[A: Decoder](name: String): Decoder.Result[Comp[A]] =
      c.get[Option[Comp[A]]](name).map(_.getOrElse(ListMap.empty))
    for {
      schemas <- getComp[SchemaLike]("schemas")
      responses <- getComp[Response]("responses")
      parameters <- getComp[Parameter]("parameters")
      examples <- getComp[Example]("examples")
      requestBodies <- getComp[RequestBody]("requestBodies")
      headers <- getComp[Header]("headers")
      securitySchemes <- getComp[SecurityScheme]("securitySchemes")
      links <- getComp[Link]("links")
      callbacks <- getComp[Callback]("callbacks")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Components(
      schemas,
      responses,
      parameters,
      examples,
      requestBodies,
      headers,
      securitySchemes,
      links,
      callbacks,
      extensions
    )
  })

  implicit val openAPIDecoder: Decoder[OpenAPI] = withExtensions(Decoder.instance { c =>
    for {
      openapi <- c.get[String]("openapi")
      info <- c.get[Info]("info")
      jsonSchemaDialect <- c.get[Option[String]]("jsonSchemaDialect")
      tags <- c.getOrElse[List[Tag]]("tags")(Nil)
      servers <- c.getOrElse[List[Server]]("servers")(Nil)
      paths <- c.getOrElse[Paths]("paths")(Paths.Empty)
      webhooks <- c.get[Option[Map[String, ReferenceOr[PathItem]]]]("webhooks")
      components <- c.get[Option[Components]]("components")
      security <- c.getOrElse[List[SecurityRequirement]]("security")(Nil)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield OpenAPI(openapi, info, jsonSchemaDialect, tags, servers, paths, webhooks, components, security, extensions)
  })

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
