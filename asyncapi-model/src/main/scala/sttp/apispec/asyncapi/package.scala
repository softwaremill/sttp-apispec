package sttp.apispec

package object asyncapi {
  type ReferenceOr[T] = Either[Reference, T]
}
