package sttp.apispec.openapi.validation

import sttp.apispec.SecurityRequirement
import sttp.apispec.openapi.{ParameterStyle, ResponsesKey}
import sttp.apispec.validation.SchemaCompatibilityIssue

sealed abstract class OpenAPICompatibilityIssue {
  def description: String

  override def toString: String = description
  protected def issuesRepr(issues: List[OpenAPICompatibilityIssue]): String =
    issues.iterator
      .map(i => s"- ${i.description.replace("\n", "\n  ")}") // indent
      .mkString("\n")
}

sealed abstract class OpenAPICompatibilitySubIssues extends OpenAPICompatibilityIssue {
  def subIssues: List[OpenAPICompatibilityIssue]
}

case class MissingPath(pathName: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing path: $pathName"
}

case class IncompatiblePath(
    pathName: String,
    subIssues: List[OpenAPICompatibilityIssue]
) extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible path $pathName:\n${issuesRepr(subIssues)}"
}

case class MissingOperation(httpMethod: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing operation for $httpMethod method"
}

case class IncompatibleOperation(
    httpMethod: String,
    subIssues: List[OpenAPICompatibilityIssue]
) extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible operation $httpMethod:\n${issuesRepr(subIssues)}"
}

case class MissingParameter(
    name: String
) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing parameter $name"
}

case class IncompatibleRequiredValue(
    clientValue: Option[Boolean],
    serverValue: Option[Boolean]
) extends OpenAPICompatibilityIssue {
  def description: String =
    s"required value mismatch: client has $clientValue, but server has $serverValue"
}

case class IncompatibleParameter(
    name: String,
    subIssues: List[OpenAPICompatibilityIssue]
) extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible parameter $name:\n${issuesRepr(subIssues)}"
}

case class IncompatibleSchema(
    schemaIssues: List[SchemaCompatibilityIssue]
) extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible schema:\n${schemaIssues}"
}

case class IncompatibleContent(
    subIssues: List[OpenAPICompatibilityIssue]
) extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible content:\n${issuesRepr(subIssues)}"
}

case class MissingMediaType(mediaType: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing media type $mediaType"
}

case class IncompatibleMediaType(mediaType: String, subIssues: List[OpenAPICompatibilityIssue])
    extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible media type $mediaType:\n${issuesRepr(subIssues)}"
}

case class IncompatibleStyle(clientValue: Option[ParameterStyle], serverValue: Option[ParameterStyle])
    extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible style value: client=$clientValue, server=$serverValue"
}

case class IncompatibleExplode(clientValue: Option[Boolean], serverValue: Option[Boolean])
    extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible explode value: client=$clientValue, server=$serverValue"
}

case class IncompatibleAllowEmptyValue(clientValue: Option[Boolean], serverValue: Option[Boolean])
    extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible allowEmptyValue value: client=$clientValue, server=$serverValue"
}

case class IncompatibleAllowReserved(clientValue: Option[Boolean], serverValue: Option[Boolean])
    extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible allowReserved value: client=$clientValue, server=$serverValue"
}

case class MissingRequestBody() extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing request body"
}

case class IncompatibleRequestBody(subIssues: List[OpenAPICompatibilityIssue]) extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible request body:\n${issuesRepr(subIssues)}"
}

case class MissingResponse(responsesKey: ResponsesKey) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing response for $responsesKey"
}

case class IncompatibleResponse(subIssues: List[OpenAPICompatibilityIssue]) extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible response:\n${issuesRepr(subIssues)}"
}

case class MissingHeader(headerName: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing header $headerName"
}

case class IncompatibleHeader(headerName: String, subIssues: List[OpenAPICompatibilityIssue])
    extends OpenAPICompatibilitySubIssues {
  def description: String =
    s"incompatible header $headerName:\n${issuesRepr(subIssues)}"
}

case class MissingSchema() extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing schema"
}

case class MissingEncoding(encodingName: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing encoding $encodingName"
}

case class IncompatibleEncoding(encodingName: String, subIssues: List[OpenAPICompatibilityIssue])
    extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible encoding $encodingName:\n${issuesRepr(subIssues)}"
}

case class IncompatibleContentType(clientValue: Option[String], serverValue: Option[String])
    extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible contentType: client=$clientValue, server=$serverValue"
}

case class IncompatibleSecurityRequirement(securityRequirement: SecurityRequirement) extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible security requirement $securityRequirement"
}
