package sttp.apispec.openapi

import sttp.apispec.AnySchema

package object circe extends SttpOpenAPICirceEncoders with SttpOpenAPICirceDecoders {
  override val anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
}

package circe {
  trait SttpOpenAPICirceEncoders extends internal.InternalSttpOpenAPICirceEncoders

  trait SttpOpenAPI3_0CirceEncoders extends internal.InternalSttpOpenAPICirceEncoders {
    override val openApi30: Boolean = true
    override val anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
  }

  trait SttpOpenAPICirceDecoders extends internal.InternalSttpOpenAPICirceDecoders
}
