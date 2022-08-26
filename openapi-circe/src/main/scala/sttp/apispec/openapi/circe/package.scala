package sttp.apispec.openapi

package object circe extends SttpOpenAPICirceEncoders with SttpOpenAPICirceDecoders

package circe {
  trait SttpOpenAPICirceEncoders extends internal.InternalSttpOpenAPICirceEncoders
  trait SttpOpenAPICirceDecoders extends internal.InternalSttpOpenAPICirceDecoders
}
