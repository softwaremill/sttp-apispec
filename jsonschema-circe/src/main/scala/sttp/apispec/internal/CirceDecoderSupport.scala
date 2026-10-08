package sttp.apispec
package internal

import io.circe.Decoder

// Only for interoperability with scala 2.
private[apispec] object CirceDecoderSupport {
  def optional[A](decoder: Decoder[A]): Decoder[Option[A]] = {
    implicit val implicitDecoder: Decoder[A] = decoder
    Decoder.decodeOption[A]
  }
}
