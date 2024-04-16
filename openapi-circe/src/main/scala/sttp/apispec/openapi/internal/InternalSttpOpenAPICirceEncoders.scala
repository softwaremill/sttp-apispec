package sttp.apispec
package openapi
package internal

import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, Json}
import sttp.apispec.internal.JsonSchemaCirceEncoders

import scala.collection.immutable.ListMap

trait InternalSttpOpenAPICirceEncoders extends JsonSchemaCirceEncoders {
  implicit val encoderReference: Encoder[Reference] = deriveEncoder[Reference]
  implicit def encoderReferenceOr[T: Encoder]: Encoder[ReferenceOr[T]] = {
    case Left(Reference(ref, summary, description)) =>
      Json
        .obj(
          s"$$ref" := ref,
          "summary" := summary,
          "description" := description
        )
        .dropNullValues
    case Right(t) => implicitly[Encoder[T]].apply(t)
  }

  implicit val encoderOAuthFlow: Encoder[OAuthFlow] = {
    // #79: all OAuth flow object MUST include a scopes field, but it MAY be empty.
    implicit def encodeListMap: Encoder[ListMap[String, String]] = doEncodeListMap(nullWhenEmpty = false)

    deriveEncoder[OAuthFlow].mapJsonObject(expandExtensions)
  }
  implicit val encoderOAuthFlows: Encoder[OAuthFlows] = deriveEncoder[OAuthFlows].mapJsonObject(expandExtensions)
  implicit val encoderSecurityScheme: Encoder[SecurityScheme] =
    deriveEncoder[SecurityScheme].mapJsonObject(expandExtensions)

  implicit val encoderHeader: Encoder[Header] = deriveEncoder[Header]
  implicit val encoderExample: Encoder[Example] = deriveEncoder[Example].mapJsonObject(expandExtensions)
  implicit val encoderResponse: Encoder[Response] = deriveEncoder[Response].mapJsonObject(expandExtensions)
  implicit val encoderLink: Encoder[Link] = deriveEncoder[Link].mapJsonObject(expandExtensions)
  implicit val encoderCallback: Encoder[Callback] = Encoder.instance { callback =>
    Json.obj(callback.pathItems.map { case (path, pathItem) => path -> pathItem.asJson }.toList: _*)
  }
  implicit val encoderEncoding: Encoder[Encoding] = deriveEncoder[Encoding].mapJsonObject(expandExtensions)
  implicit val encoderMediaType: Encoder[MediaType] = deriveEncoder[MediaType].mapJsonObject(expandExtensions)
  implicit val encoderRequestBody: Encoder[RequestBody] = deriveEncoder[RequestBody].mapJsonObject(expandExtensions)
  implicit val encoderParameterStyle: Encoder[ParameterStyle] = { e => Encoder.encodeString(e.value) }
  implicit val encoderParameterIn: Encoder[ParameterIn] = { e => Encoder.encodeString(e.value) }
  implicit val encoderParameter: Encoder[Parameter] = deriveEncoder[Parameter].mapJsonObject(expandExtensions)
  implicit val encoderResponseMap: Encoder[ListMap[ResponsesKey, ReferenceOr[Response]]] =
    (responses: ListMap[ResponsesKey, ReferenceOr[Response]]) => {
      val fields = responses.map {
        case (ResponsesDefaultKey, r)      => ("default", r.asJson)
        case (ResponsesCodeKey(code), r)   => (code.toString, r.asJson)
        case (ResponsesRangeKey(range), r) => (s"${range}XX", r.asJson)
      }

      Json.obj(fields.toSeq: _*)
    }
  implicit val encoderResponses: Encoder[Responses] = Encoder.instance { resp =>
    val extensions = resp.extensions.asJsonObject
    val respJson = resp.responses.asJson
    respJson.asObject.map(_.deepMerge(extensions).asJson).getOrElse(respJson)
  }

  implicit val encoderOperation: Encoder[Operation] = {
    // this is needed to override the encoding of `security: List[SecurityRequirement]`. An empty security requirement
    // should be represented as an empty object (`{}`), not `null`, which is the default encoding of `ListMap`s.
    implicit def encodeListMap[V: Encoder]: Encoder[ListMap[String, V]] = doEncodeListMap(nullWhenEmpty = false)

    implicit def encodeListMapForCallbacks: Encoder[ListMap[String, ReferenceOr[Callback]]] =
      doEncodeListMap(nullWhenEmpty = true)

    deriveEncoder[Operation].mapJsonObject(expandExtensions)
  }
  implicit val encoderPathItem: Encoder[PathItem] = deriveEncoder[PathItem].mapJsonObject(expandExtensions)
  implicit val encoderPaths: Encoder[Paths] = Encoder.instance { paths =>
    val extensions = paths.extensions.asJsonObject
    val pathItems = paths.pathItems.asJson
    pathItems.asObject.map(_.deepMerge(extensions).asJson).getOrElse(pathItems)
  }
  implicit val encoderComponents: Encoder[Components] = deriveEncoder[Components].mapJsonObject(expandExtensions)
  implicit val encoderServerVariable: Encoder[ServerVariable] =
    deriveEncoder[ServerVariable].mapJsonObject(expandExtensions)
  implicit val encoderServer: Encoder[Server] = deriveEncoder[Server].mapJsonObject(expandExtensions)
  implicit val encoderTag: Encoder[Tag] = deriveEncoder[Tag].mapJsonObject(expandExtensions)
  implicit val encoderInfo: Encoder[Info] = deriveEncoder[Info].mapJsonObject(expandExtensions)
  implicit val encoderContact: Encoder[Contact] = deriveEncoder[Contact].mapJsonObject(expandExtensions)
  implicit val encoderLicense: Encoder[License] = deriveEncoder[License].mapJsonObject(expandExtensions)
  implicit val encoderOpenAPI: Encoder[OpenAPI] =
    deriveEncoder[OpenAPI].mapJsonObject(expandExtensions).mapJson(_.deepDropNullValues)
}
