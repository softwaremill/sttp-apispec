package sttp.apispec.openapi

import scala.collection.immutable.ListMap

sealed trait APICompatibilityIssue

case object MissingPath extends APICompatibilityIssue
case object UnexpectedPath extends APICompatibilityIssue
case class MissingOperation(method: String) extends APICompatibilityIssue
case class UnexpectedOperation(method: String) extends APICompatibilityIssue
case class ParameterNameMismatch(thisName: String, referenceName: String) extends APICompatibilityIssue
case class ParameterInMismatch(thisIn: ParameterIn, referenceIn: ParameterIn) extends APICompatibilityIssue

case class APICompatibilityIssue2(pathIssues: List[PathIssue])
case class PathIssue(path: String, issues: List[APICompatibilityIssue])

object OpenAPIComparator {

  def compare(thisAPI: OpenAPI, referenceAPI: OpenAPI): APICompatibilityIssue2 =
    APICompatibilityIssue2(pathIssues = comparePaths(thisAPI.paths.pathItems, referenceAPI.paths.pathItems))

  private[openapi] def comparePaths(
      thisPathItems: ListMap[String, PathItem],
      referencePathItems: ListMap[String, PathItem]
  ): List[PathIssue] = {
    comparePathsNames(thisPathItems.keys.toSet, referencePathItems.keys.toSet) ++
      comparePathsItems(thisPathItems, referencePathItems)
  }

  private def comparePathsNames(
      thisPathsNames: Set[String],
      referencePathsNames: Set[String]
  ): List[PathIssue] = {
    val pathsInThisButNotInReference = referencePathsNames.diff(thisPathsNames)
    val pathsInReferenceButNotInThis = thisPathsNames.diff(referencePathsNames)
    pathsInThisButNotInReference.map(path => PathIssue(path, List(MissingPath))).toList ++
      pathsInReferenceButNotInThis.map(path => PathIssue(path, List(UnexpectedPath))).toList
  }

  private def comparePathsItems(
      thisPaths: ListMap[String, PathItem],
      referencePaths: ListMap[String, PathItem]
  ): List[PathIssue] = {
    val validPaths = thisPaths.keys.toSet.intersect(referencePaths.keys.toSet).toList
    validPaths
      .map(path => PathIssue(path, comparePathItem(thisPaths(path), referencePaths(path))))
      .filter(_.issues.nonEmpty)
  }

  private def comparePathItem(
      thisPathItem: PathItem,
      referencePathItem: PathItem
  ): List[APICompatibilityIssue] = {
    List(
      compareOperation(thisPathItem, referencePathItem, "get", _.get),
      compareOperation(thisPathItem, referencePathItem, "put", _.put),
      compareOperation(thisPathItem, referencePathItem, "post", _.post),
      compareOperation(thisPathItem, referencePathItem, "delete", _.delete),
      compareOperation(thisPathItem, referencePathItem, "options", _.options),
      compareOperation(thisPathItem, referencePathItem, "head", _.head),
      compareOperation(thisPathItem, referencePathItem, "patch", _.patch),
      compareOperation(thisPathItem, referencePathItem, "trace", _.trace)
    ).flatten
  }

  private def compareOperation(
      thisPathItem: PathItem,
      referencePathItem: PathItem,
      method: String,
      operationExtractor: PathItem => Option[Operation]
  ): List[APICompatibilityIssue] = {
    val thisOperation = operationExtractor(thisPathItem)
    val referenceOperation = operationExtractor(referencePathItem)
    (thisOperation, referenceOperation) match {
      case (Some(thisOp), Some(refOp)) => compareOperationSpec(thisOp, refOp)
      case (Some(_), None)             => List(UnexpectedOperation(method))
      case (None, Some(_))             => List(MissingOperation(method))
      case (None, None)                => Nil
    }
  }

  private def compareOperationSpec(
      thisOp: Operation,
      refOp: Operation
  ): List[APICompatibilityIssue] = {
    compareParameters(thisOp.parameters, refOp.parameters) ++
      compareRequestBody(thisOp.requestBody, refOp.requestBody) ++
      compareResponses(thisOp.responses.responses, refOp.responses.responses)
  }

  private def compareParameters(
      thisParameters: List[ReferenceOr[Parameter]],
      referenceParameters: List[ReferenceOr[Parameter]]
  ): List[APICompatibilityIssue] = thisParameters
    .zip(referenceParameters)
    .flatMap { case (thisParam, refParam) =>
      compareParameters(thisParam, refParam)
    }

  private def compareParameters(
      thisParameter: ReferenceOr[Parameter],
      referenceParameter: ReferenceOr[Parameter]
  ): List[APICompatibilityIssue] = {
    (thisParameter, referenceParameter) match {
      case (Right(thisParam), Right(refParam)) => compareParameters(thisParam, refParam)
      case _                                   => throw new IllegalStateException("not implemented")
    }
  }

  private def compareParameters(
      thisParameter: Parameter,
      referenceParameter: Parameter
  ): List[APICompatibilityIssue] = {
    List(
      if (thisParameter.name != referenceParameter.name)
        Some(ParameterNameMismatch(thisParameter.name, referenceParameter.name))
      else None,
      if (thisParameter.in != referenceParameter.in)
        Some(ParameterInMismatch(thisParameter.in, referenceParameter.in))
      else None
    ).flatten
  }

  private def compareRequestBody(
      thisRequestBody: Option[ReferenceOr[RequestBody]],
      referenceRequestBody: Option[ReferenceOr[RequestBody]]
  ): List[APICompatibilityIssue] = Nil

  private def compareResponses(
      thisResponses: ListMap[ResponsesKey, ReferenceOr[Response]],
      referenceResponses: ListMap[ResponsesKey, ReferenceOr[Response]]
  ): List[APICompatibilityIssue] = Nil
}
