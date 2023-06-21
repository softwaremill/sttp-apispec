package sttp.apispec.openapi.circe.yaml

import io.circe.syntax._
import io.circe.yaml.Printer
import io.circe.yaml.Printer.StringStyle
import sttp.apispec.openapi.OpenAPI

trait SttpOpenAPICirceYaml {

  implicit class RichOpenAPI(openAPI: OpenAPI) {

    /** Converts `OpenAPI` to Open Api 3.0.3 YAML string */
    def toYaml3_0_3: String = {
      import sttp.apispec.openapi.circe_openapi_3_0_3._
      Printer(dropNullKeys = true, preserveOrder = true).pretty(openAPI.asJson)
    }

    /** Converts `OpenAPI` to Open Api 3.0.3 YAML string */
    def toYaml3_0_3(stringStyle: StringStyle): String = {
      import sttp.apispec.openapi.circe_openapi_3_0_3._
      Printer(dropNullKeys = true, preserveOrder = true, stringStyle = stringStyle).pretty(openAPI.asJson)
    }

    /** Converts `OpenAPI` to Open Api 3.1.0 YAML string */
    def toYaml: String = {
      import sttp.apispec.openapi.circe._
      Printer(dropNullKeys = true, preserveOrder = true).pretty(openAPI.asJson)
    }

    /** Converts `OpenAPI` to Open Api 3.1.0 YAML string */
    def toYaml(stringStyle: StringStyle): String = {
      import sttp.apispec.openapi.circe._
      Printer(dropNullKeys = true, preserveOrder = true, stringStyle = stringStyle).pretty(openAPI.asJson)
    }
  }
}
