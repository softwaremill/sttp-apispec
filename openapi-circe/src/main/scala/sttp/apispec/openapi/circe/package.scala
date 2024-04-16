package sttp.apispec.openapi

import sttp.apispec.AnySchema
import sttp.apispec.openapi.circe.{SttpOpenAPICirceDecoders, SttpOpenAPI3_0_3CirceEncoders}

package object circe_openapi_3_0_3 extends SttpOpenAPI3_0_3CirceEncoders with SttpOpenAPICirceDecoders {
  override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
}

package object circe extends SttpOpenAPICirceEncoders with SttpOpenAPICirceDecoders {
  override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
}

package circe {
  trait SttpOpenAPI3_0_3CirceEncoders extends internal.InternalSttpOpenAPICirceEncoders {
    override def openApi30: Boolean = true
  }

  trait SttpOpenAPICirceEncoders extends internal.InternalSttpOpenAPICirceEncoders {
    override def openApi30: Boolean = false
    override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
  }

  trait SttpOpenAPICirceDecoders extends internal.InternalSttpOpenAPICirceDecoders
}
