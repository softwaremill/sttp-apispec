package sttp.apispec.openapi

import sttp.apispec.AnySchema

package object circe extends SttpOpenAPICirceEncoders with SttpOpenAPICirceDecoders {
  val anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
}

package circe {
  trait SttpOpenAPICirceEncoders extends internal.InternalSttpOpenAPICirceEncoders
  trait SttpOpenAPICirceDecoders extends internal.InternalSttpOpenAPICirceDecoders
}
