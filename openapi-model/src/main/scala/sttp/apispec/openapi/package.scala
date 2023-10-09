package sttp.apispec

package object openapi {
  type ReferenceOr[T] = Either[Reference, T]
}
