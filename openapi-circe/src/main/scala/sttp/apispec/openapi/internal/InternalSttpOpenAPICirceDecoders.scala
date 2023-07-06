package sttp.apispec
package openapi
package internal

import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto.deriveDecoder
import sttp.apispec.internal.JsonSchemaCirceDecoders

import scala.collection.immutable.ListMap

trait InternalSttpOpenAPICirceDecoders extends JsonSchemaCirceDecoders {

  implicit val externalDocumentationDecoder: Decoder[ExternalDocumentation] = withExtensions(
    deriveDecoder[ExternalDocumentation]
  )
  implicit val tagDecoder: Decoder[Tag] = withExtensions(deriveDecoder[Tag])

  implicit val oauthFlowDecoder: Decoder[OAuthFlow] = {
    // #79: all OAuth flow object MUST include a scopes field, but it MAY be empty.
    implicit def listMapDecoder: Decoder[ListMap[String, String]] =
      Decoder.decodeOption(Decoder.decodeMapLike[String, String, ListMap]).map(_.getOrElse(ListMap.empty))

    withExtensions(deriveDecoder[OAuthFlow])
  }
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
    val ResponseRange = "(1|2|3|4|5)XX".r
    val ResponseCode = "([1|2|3|4|5]\\d\\d)".r
    KeyDecoder.decodeKeyString.map {
      case "default"            => ResponsesDefaultKey
      case ResponseRange(range) => ResponsesRangeKey(range.toInt)
      case ResponseCode(code)   => ResponsesCodeKey(code.toInt)
      case key                  => sys.error(s"'$key' Not a valid responsekey")
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
}
