package sttp.apispec.openapi

package object circe extends SttpOpenAPICirceEncoders

package circe {
  trait SttpOpenAPICirceEncoders extends internal.InternalSttpOpenAPICirceEncoders
}
