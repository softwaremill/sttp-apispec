package sttp.apispec
package openapi
package internal

import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Encoder, Json, JsonObject}

import scala.collection.immutable.ListMap

trait InternalSttpOpenAPICirceEncoders {
  // note: these are strict val-s, order matters!
  implicit def encoderReferenceOr[T: Encoder]: Encoder[ReferenceOr[T]] = {
    case Left(Reference(ref, summary, description)) =>
      Json
        .obj(
          "$ref" := ref,
          "summary" := summary,
          "description" := description
        )
        .dropNullValues
    case Right(t) => implicitly[Encoder[T]].apply(t)
  }

  implicit val extensionValue: Encoder[ExtensionValue] =
    Encoder.instance(e => parse(e.value).getOrElse(Json.fromString(e.value)))
  implicit val encoderOAuthFlow: Encoder[OAuthFlow] = deriveEncoder[OAuthFlow].mapJsonObject(expandExtensions)
  implicit val encoderOAuthFlows: Encoder[OAuthFlows] = deriveEncoder[OAuthFlows].mapJsonObject(expandExtensions)
  implicit val encoderSecurityScheme: Encoder[SecurityScheme] =
    deriveEncoder[SecurityScheme].mapJsonObject(expandExtensions)
  implicit val encoderExampleSingleValue: Encoder[ExampleSingleValue] = {
    case ExampleSingleValue(value: String) => parse(value).getOrElse(Json.fromString(value))
    case ExampleSingleValue(value: Int) => Json.fromInt(value)
    case ExampleSingleValue(value: Long) => Json.fromLong(value)
    case ExampleSingleValue(value: Float) => Json.fromFloatOrString(value)
    case ExampleSingleValue(value: Double) => Json.fromDoubleOrString(value)
    case ExampleSingleValue(value: Boolean) => Json.fromBoolean(value)
    case ExampleSingleValue(value: BigDecimal) => Json.fromBigDecimal(value)
    case ExampleSingleValue(value: BigInt) => Json.fromBigInt(value)
    case ExampleSingleValue(null) => Json.Null
    case ExampleSingleValue(value) => Json.fromString(value.toString)
  }
  implicit val encoderExampleValue: Encoder[ExampleValue] = {
    case e: ExampleSingleValue =>
      Json.arr(encoderExampleSingleValue(e))
    case ExampleMultipleValue(values) =>
      Json.arr(values.map(v => encoderExampleSingleValue(ExampleSingleValue(v))): _*)
  }
  implicit val encoderSchemaType: Encoder[SchemaType] = {
    case e: BasicSchemaType => e.value.asJson
    case ArraySchemaType(typ) => typ.map(_.value.asJson).asJson
  }
  implicit val encoderDiscriminator: Encoder[Discriminator] = deriveEncoder[Discriminator]

  implicit val encoderSchema: Encoder[Schema] = Encoder.AsObject
    .instance { (s: Schema) =>
      val minKey = if (s.exclusiveMinimum.getOrElse(false)) "exclusiveMinimum" else "minimum"
      val maxKey = if (s.exclusiveMaximum.getOrElse(false)) "exclusiveMaximum" else "maximum"
      JsonObject(
        "$schema" := s.$schema,
        "allOf" := s.allOf,
        "title" := s.title,
        "required" := s.required,
        "type" := (if (s.nullable.getOrElse(false))
          s.`type`.map(s => Json.arr(s.asJson, Json.fromString("null"))).asJson
        else s.`type`.asJson),
        "items" := s.items,
        "properties" := s.properties,
        "description" := s.description,
        "format" := s.format,
        "default" := s.default,
        "readOnly" := s.readOnly,
        "writeOnly" := s.writeOnly,
        "examples" := s.example,
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
        "extensions" := s.extensions
      )
    }
    .mapJsonObject(expandExtensions)

  implicit val encoderAnySchema: Encoder[AnySchema] = Encoder.instance {
    case AnySchema.Anything(AnySchema.Encoding.Object) => Json.obj()
    case AnySchema.Anything(AnySchema.Encoding.Boolean) => Json.True
    case AnySchema.Nothing(AnySchema.Encoding.Object) => Json.obj(
      "not" := Json.obj()
    )
    case AnySchema.Nothing(AnySchema.Encoding.Boolean) => Json.False
  }

  implicit val encoderSchemaLike: Encoder[SchemaLike] = Encoder.instance {
    case s: AnySchema => encoderAnySchema(s)
    case s: Schema => encoderSchema(s)
  }

  implicit val encoderReference: Encoder[Reference] = deriveEncoder[Reference]
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
        case (ResponsesDefaultKey, r) => ("default", r.asJson)
        case (ResponsesCodeKey(code), r) => (code.toString, r.asJson)
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
  implicit val encoderExternalDocumentation: Encoder[ExternalDocumentation] =
    deriveEncoder[ExternalDocumentation].mapJsonObject(expandExtensions)
  implicit val encoderTag: Encoder[Tag] = deriveEncoder[Tag].mapJsonObject(expandExtensions)
  implicit val encoderInfo: Encoder[Info] = deriveEncoder[Info].mapJsonObject(expandExtensions)
  implicit val encoderContact: Encoder[Contact] = deriveEncoder[Contact].mapJsonObject(expandExtensions)
  implicit val encoderLicense: Encoder[License] = deriveEncoder[License].mapJsonObject(expandExtensions)
  implicit val encoderOpenAPI: Encoder[OpenAPI] =
    deriveEncoder[OpenAPI].mapJsonObject(expandExtensions).mapJson(_.deepDropNullValues)

  implicit def encodeList[T: Encoder]: Encoder[List[T]] = {
    case Nil => Json.Null
    case l: List[T] => Json.arr(l.map(i => implicitly[Encoder[T]].apply(i)): _*)
  }

  implicit def encodeListMap[V: Encoder]: Encoder[ListMap[String, V]] = doEncodeListMap(nullWhenEmpty = true)

  private def doEncodeListMap[V: Encoder](nullWhenEmpty: Boolean): Encoder[ListMap[String, V]] = {
    case m: ListMap[String, V] if m.isEmpty && nullWhenEmpty => Json.Null
    case m: ListMap[String, V] =>
      val properties = m.mapValues(v => implicitly[Encoder[V]].apply(v)).toList
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
  private def expandExtensions(jsonObject: JsonObject): JsonObject = {
    val extensions = jsonObject("extensions")
    val jsonWithoutExt = jsonObject.filterKeys(_ != "extensions")
    extensions.flatMap(_.asObject).map(extObject => extObject.deepMerge(jsonWithoutExt)).getOrElse(jsonWithoutExt)
  }
}
