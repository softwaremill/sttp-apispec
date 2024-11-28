package sttp.apispec.openapi.validation

import sttp.apispec.validation.SchemaCompatibilityIssue

sealed abstract class OpenAPICompatibilityIssue {
  def description: String

  override def toString: String = description
  protected def issuesRepr(issues: List[OpenAPICompatibilityIssue]): String =
    issues.iterator
      .map(i => s"- ${i.description.replace("\n", "\n  ")}") // indent
      .mkString("\n")
}

sealed abstract class SubOpenAPICompatibilityIssue extends OpenAPICompatibilityIssue {
  def subIssues: List[OpenAPICompatibilityIssue]
}

case class MissingPath(pathName: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing path: $pathName"
}

case class IncompatiblePath(
    pathName: String,
    subIssues: List[OpenAPICompatibilityIssue]
) extends SubOpenAPICompatibilityIssue {
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
) extends SubOpenAPICompatibilityIssue {
  def description: String =
    s"incompatible operation $httpMethod:\n${issuesRepr(subIssues)}"
}

case class MissingParameter(
    name: String
) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing parameter $name"
}

case class IncompatibleParameter(
    name: String,
    subIssues: List[OpenAPICompatibilityIssue]
) extends SubOpenAPICompatibilityIssue {
  def description: String =
    s"incompatible parameter $name:\n${issuesRepr(subIssues)}"
}

case class IncompatibleSchema(
    schemaIssues: List[SchemaCompatibilityIssue]
) extends OpenAPICompatibilityIssue {
  def description: String =
    s"incompatible schema:\n${schemaIssues}"
}

case class IncompatibleParameterContent(
    subIssues: List[OpenAPICompatibilityIssue]
) extends SubOpenAPICompatibilityIssue {
  def description: String =
    s"incompatible parameter content:\n${issuesRepr(subIssues)}"
}

case class MissingMediaType(mediaType: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"missing media type $mediaType"
}

case class IncompatibleMediaType(mediaType: String, subIssues: List[OpenAPICompatibilityIssue])
    extends SubOpenAPICompatibilityIssue {
  def description: String =
    s"incompatible media type $mediaType:\n${issuesRepr(subIssues)}"
}

case class MissMatch(name: String) extends OpenAPICompatibilityIssue {
  def description: String =
    s"miss match $name"
}
