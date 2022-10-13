package sttp.apispec.openapi.circe.yaml

import io.circe.syntax._
import io.circe.yaml.Printer
import io.circe.yaml.Printer.StringStyle
import sttp.apispec.openapi.OpenAPI

trait SttpOpenAPICirceYaml {

  implicit class RichOpenAPI(openAPI: OpenAPI) {
    import sttp.apispec.openapi.circe._

    def toYaml: String = Printer(dropNullKeys = true, preserveOrder = true).pretty(openAPI.asJson)
    def toYaml(stringStyle: StringStyle): String =
      Printer(dropNullKeys = true, preserveOrder = true, stringStyle = stringStyle).pretty(openAPI.asJson)
  }

  implicit class RichOpenAPI3_1(openAPI: OpenAPI) {
    import sttp.apispec.openapi.circe_openapi_3_1._

    def toYaml: String = Printer(dropNullKeys = true, preserveOrder = true).pretty(openAPI.asJson)
    def toYaml(stringStyle: StringStyle): String =
      Printer(dropNullKeys = true, preserveOrder = true, stringStyle = stringStyle).pretty(openAPI.asJson)
  }
}
