package sttp.apispec
package asyncapi

import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import io.circe._

package object circe extends SttpAsyncAPICirceEncoders {
  val anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
}

package circe {

  import sttp.apispec.internal.JsonSchemaCirceEncoders

  trait SttpAsyncAPICirceEncoders extends JsonSchemaCirceEncoders {
    // note: these are strict val-s, order matters!
    override val openApi30: Boolean = true

    implicit val encoderOAuthFlow: Encoder[OAuthFlow] = {
      import scala.collection.immutable.ListMap
      implicit def encodeListMap: Encoder[ListMap[String, String]] = doEncodeListMap(nullWhenEmpty = false)

      deriveEncoder[OAuthFlow].mapJsonObject(expandExtensions)
    }

    implicit val encoderOAuthFlows: Encoder[OAuthFlows] = deriveEncoder[OAuthFlows].mapJsonObject(expandExtensions)
    implicit val encoderSecurityScheme: Encoder[SecurityScheme] =
      deriveEncoder[SecurityScheme].mapJsonObject(expandExtensions)

    implicit val encoderExternalDocumentation: Encoder[ExternalDocumentation] =
      deriveEncoder[ExternalDocumentation].mapJsonObject(expandExtensions)
    implicit val encoderTag: Encoder[Tag] = deriveEncoder[Tag].mapJsonObject(expandExtensions)

    implicit val encoderAnyValue: Encoder[AnyValue] = (av: AnyValue) => {
      parse(av.value).getOrElse(Json.fromString(av.value))
    }
    implicit val encoderCorrelationId: Encoder[CorrelationId] =
      deriveEncoder[CorrelationId].mapJsonObject(expandExtensions)
    implicit val encoderParameter: Encoder[Parameter] = deriveEncoder[Parameter].mapJsonObject(expandExtensions)

    implicit val encoderMessageBinding: Encoder[List[MessageBinding]] = {
      implicit val encoderHttpMessageBinding: Encoder[HttpMessageBinding] = deriveEncoder[HttpMessageBinding]
      implicit val encoderWebSocketMessageBinding: Encoder[WebSocketMessageBinding] =
        deriveEncoder[WebSocketMessageBinding]
      implicit val encoderKafkaMessageBinding: Encoder[KafkaMessageBinding] = deriveEncoder[KafkaMessageBinding]
      (a: List[MessageBinding]) =>
        nullIfEmpty(a)(
          Json.obj(
            a.map {
              case v: HttpMessageBinding      => "http" -> v.asJson
              case v: WebSocketMessageBinding => "ws" -> v.asJson
              case v: KafkaMessageBinding     => "kafka" -> v.asJson
            }: _*
          )
        )
    }

    implicit val encoderOperationBinding: Encoder[List[OperationBinding]] = {
      implicit val encoderHttpOperationBinding: Encoder[HttpOperationBinding] = deriveEncoder[HttpOperationBinding]
      implicit val encoderWebSocketOperationBinding: Encoder[WebSocketOperationBinding] =
        deriveEncoder[WebSocketOperationBinding]
      implicit val encoderKafkaOperationBinding: Encoder[KafkaOperationBinding] = deriveEncoder[KafkaOperationBinding]
      (a: List[OperationBinding]) =>
        nullIfEmpty(a)(
          Json.obj(
            a.map {
              case v: HttpOperationBinding      => "http" -> v.asJson
              case v: WebSocketOperationBinding => "ws" -> v.asJson
              case v: KafkaOperationBinding     => "kafka" -> v.asJson
            }: _*
          )
        )
    }

    implicit val encoderChannelBinding: Encoder[List[ChannelBinding]] = {
      implicit val encoderHttpChannelBinding: Encoder[HttpChannelBinding] = deriveEncoder[HttpChannelBinding]
      implicit val encoderWebSocketChannelBinding: Encoder[WebSocketChannelBinding] =
        deriveEncoder[WebSocketChannelBinding]
      implicit val encoderKafkaChannelBinding: Encoder[KafkaChannelBinding] = deriveEncoder[KafkaChannelBinding]
      (a: List[ChannelBinding]) =>
        nullIfEmpty(a)(
          Json.obj(
            a.map {
              case v: HttpChannelBinding      => "http" -> v.asJson
              case v: WebSocketChannelBinding => "ws" -> v.asJson
              case v: KafkaChannelBinding     => "kafka" -> v.asJson
            }: _*
          )
        )
    }

    implicit val encoderServerBinding: Encoder[List[ServerBinding]] = {
      implicit val encoderHttpServerBinding: Encoder[HttpServerBinding] = deriveEncoder[HttpServerBinding]
      implicit val encoderWebSocketServerBinding: Encoder[WebSocketServerBinding] =
        deriveEncoder[WebSocketServerBinding]
      implicit val encoderKafkaServerBinding: Encoder[KafkaServerBinding] = deriveEncoder[KafkaServerBinding]
      (a: List[ServerBinding]) =>
        nullIfEmpty(a)(
          Json.obj(
            a.map {
              case v: HttpServerBinding      => "http" -> v.asJson
              case v: WebSocketServerBinding => "ws" -> v.asJson
              case v: KafkaServerBinding     => "kafka" -> v.asJson
            }: _*
          )
        )
    }

    private def nullIfEmpty[T](a: List[T])(otherwise: => Json): Json = if (a.isEmpty) Json.Null else otherwise

    implicit val encoderMessagePayload: Encoder[Option[Either[AnyValue, ReferenceOr[Schema]]]] = {
      case None           => Json.Null
      case Some(Left(av)) => encoderAnyValue.apply(av)
      case Some(Right(s)) => encoderReferenceOr[Schema].apply(s)
    }

    implicit val encoderMessageTrait: Encoder[MessageTrait] =
      deriveEncoder[MessageTrait].mapJsonObject(expandExtensions)
    implicit val encoderSingleMessage: Encoder[SingleMessage] =
      deriveEncoder[SingleMessage].mapJsonObject(expandExtensions)
    implicit val encoderOneOfMessage: Encoder[OneOfMessage] = deriveEncoder[OneOfMessage]
    implicit val encoderMessage: Encoder[Message] = {
      case s: SingleMessage => encoderSingleMessage.apply(s)
      case o: OneOfMessage  => encoderOneOfMessage.apply(o)
    }

    implicit val encoderOperationTrait: Encoder[OperationTrait] =
      deriveEncoder[OperationTrait].mapJsonObject(expandExtensions)
    implicit val encoderOperation: Encoder[Operation] = deriveEncoder[Operation].mapJsonObject(expandExtensions)
    implicit val encoderChannelItem: Encoder[ChannelItem] = deriveEncoder[ChannelItem].mapJsonObject(expandExtensions)
    implicit val encoderComponents: Encoder[Components] = deriveEncoder[Components].mapJsonObject(expandExtensions)
    implicit val encoderServerVariable: Encoder[ServerVariable] =
      deriveEncoder[ServerVariable].mapJsonObject(expandExtensions)
    implicit val encoderServer: Encoder[Server] = deriveEncoder[Server].mapJsonObject(expandExtensions)
    implicit val encoderContact: Encoder[Contact] = deriveEncoder[Contact].mapJsonObject(expandExtensions)
    implicit val encoderLicense: Encoder[License] = deriveEncoder[License].mapJsonObject(expandExtensions)
    implicit val encoderInfo: Encoder[Info] = deriveEncoder[Info].mapJsonObject(expandExtensions)
    implicit val encoderAsyncAPI: Encoder[AsyncAPI] = deriveEncoder[AsyncAPI].mapJsonObject(expandExtensions)
  }
}
