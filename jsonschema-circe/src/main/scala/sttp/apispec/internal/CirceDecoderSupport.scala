package sttp.apispec
package internal

import io.circe.Decoder

private[apispec] object CirceDecoderSupport {
  def optional[A](decoder: Decoder[A]): Decoder[Option[A]] = {
    implicit val implicitDecoder: Decoder[A] = decoder
    Decoder.decodeOption[A]
  }
}
