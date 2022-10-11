package sttp.apispec

import sttp.apispec.AnySchema.Encoding.Boolean
import sttp.apispec.internal.{JsonSchemaCirceDecoders, JsonSchemaCirceEncoders}

object circe extends JsonSchemaCirceEncoders with JsonSchemaCirceDecoders {
  override def anyObjectEncoding: AnySchema.Encoding = Boolean
}
