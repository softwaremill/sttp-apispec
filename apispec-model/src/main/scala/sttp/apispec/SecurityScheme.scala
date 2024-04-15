package sttp.apispec

import scala.collection.immutable.ListMap

case class SecurityScheme(
    `type`: String,
    description: Option[String] = None,
    name: Option[String] = None,
    in: Option[String] = None,
    scheme: Option[String] = None,
    bearerFormat: Option[String] = None,
    flows: Option[OAuthFlows] = None,
    openIdConnectUrl: Option[String] = None,
    extensions: ListMap[String, ExtensionValue] = ListMap.empty
)

case class OAuthFlows(
    `implicit`: Option[OAuthFlow] = None,
    password: Option[OAuthFlow] = None,
    clientCredentials: Option[OAuthFlow] = None,
    authorizationCode: Option[OAuthFlow] = None,
    extensions: ListMap[String, ExtensionValue] = ListMap.empty
)

case class OAuthFlow(
    authorizationUrl: Option[String] = None,
    tokenUrl: Option[String] = None,
    refreshUrl: Option[String] = None,
    scopes: ListMap[String, String] = ListMap.empty,
    extensions: ListMap[String, ExtensionValue] = ListMap.empty
)
