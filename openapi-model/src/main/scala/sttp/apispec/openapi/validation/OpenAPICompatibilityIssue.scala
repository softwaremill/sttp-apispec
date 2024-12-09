package sttp.apispec.openapi.validation

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

case class IncompatibleRequiredParameter(
    name: String
) extends OpenAPICompatibilityIssue {
  def description: String =
    s"parameter '$name' is required by the client but optional on the server"
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

case class IncompatibleStyle(clientValue: Option[ParameterStyle], serverValue: Option[ParameterStyle]) extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible style value: client=$clientValue, server=$serverValue"
}

case class IncompatibleExplode(clientValue: Option[Boolean], serverValue: Option[Boolean]) extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible explode value: client=$clientValue, server=$serverValue"
}

case class IncompatibleAllowEmptyValue(clientValue: Option[Boolean], serverValue: Option[Boolean]) extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible allowEmptyValue value: client=$clientValue, server=$serverValue"
}

case class IncompatibleAllowReserved(clientValue: Option[Boolean], serverValue: Option[Boolean]) extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible allowReserved value: client=$clientValue, server=$serverValue"
}

case class MissingRequestBody() extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing request body"
}

case class IncompatibleRequiredRequestBody() extends OpenAPICompatibilityIssue {
  def description: String =
    s"request body is required by the client but optional on the server"
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

case class IncompatibleRequiredHeader(headerName: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"header '$headerName' is required by the client but optional on the server"
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