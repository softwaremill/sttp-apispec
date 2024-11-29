package sttp.apispec.openapi.validation

import sttp.apispec.{Schema, SchemaLike}
import sttp.apispec.openapi.{Header, MediaType, OpenAPI, Operation, Parameter, PathItem, RequestBody, Response}
import sttp.apispec.validation.SchemaComparator

import scala.collection.immutable.ListMap

class OpenAPIComparator(
    writerOpenAPI: OpenAPI,
    readerOpenAPI: OpenAPI
) {
  private val httpMethods = List("get", "post", "patch", "delete", "options", "trace", "head", "put")

  def compare(): List[OpenAPICompatibilityIssue] = {
    writerOpenAPI.paths.pathItems.toList.flatMap {
      case (pathName, writerPathItem) =>
        val readerPathItem = readerOpenAPI.paths.pathItems.get(pathName)
        readerPathItem match {
          case None => Some(MissingPath(pathName))
          case Some(readerPathItem) =>
            val pathIssues = checkPath(pathName, writerPathItem, readerPathItem)
            if (pathIssues.isEmpty)
              None
            else
              pathIssues
          case _ => None
        }
      case _ => None
    }
  }

  private def checkPath(
      pathName: String,
      writerPathItem: PathItem,
      readerPathItem: PathItem
  ): Option[IncompatiblePath] = {
    val issues = httpMethods.flatMap { httpMethod =>
      val writerOperation = getOperation(writerPathItem, httpMethod)
      val readerOperation = getOperation(readerPathItem, httpMethod)

      (readerOperation, writerOperation) match {
        case (None, Some(_))                  => Some(MissingOperation(httpMethod))
        case (Some(readerOp), Some(writerOp)) => checkOperation(httpMethod, writerOp, readerOp)
        case _                                => None
      }
    }
    if (issues.isEmpty)
      None
    else
      Some(IncompatiblePath(pathName, issues))
  }
  private def getOperation(pathItem: PathItem, httpMethod: String): Option[Operation] = httpMethod match {
    case "get"     => pathItem.get
    case "patch"   => pathItem.patch
    case "delete"  => pathItem.delete
    case "options" => pathItem.options
    case "trace"   => pathItem.trace
    case "head"    => pathItem.head
    case "post"    => pathItem.post
    case "put"     => pathItem.put
    case _         => None
  }

  private def checkOperation(
      httpMethod: String,
      writerOperation: Operation,
      readerOperation: Operation
  ): Option[IncompatibleOperation] = {
    val readerParameters = getOperationParameters(readerOperation)
    val writerParameters = getOperationParameters(writerOperation)

    val parametersIssue = writerParameters.flatMap { writerParameter =>
      val readerParameter = readerParameters.find(_.name == writerParameter.name)
      readerParameter match {
        case None                  => Some(MissingParameter(writerParameter.name))
        case Some(readerParameter) => checkParameter(writerParameter, readerParameter)
      }
    }

    val requestBodyIssue = (writerOperation.requestBody, readerOperation.requestBody) match {
      case (Some(Right(writerRequestBody)), Some(Right(readerRequestBody))) =>
        checkRequestBody(writerRequestBody, readerRequestBody)
      case (Some(Right(_)), None) => Some(MissingRequestBody())
      case _                      => None
    }

    val responsesIssues = writerOperation.responses.responses.flatMap {
      case (writerResponseKey, Right(writerResponse)) =>
        val readerResponse = readerOperation.responses.responses.get(writerResponseKey)
        readerResponse match {
          case Some(Right(readerResponse)) => checkResponse(writerResponse, readerResponse)
          case None                        => Some(MissingResponse(writerResponseKey))
          case _                           => None
        }
      case _ => None
    }

    // TODO: callbacks, security?
    val issues = parametersIssue ++ requestBodyIssue ++ responsesIssues
    if (issues.isEmpty)
      None
    else
      Some(IncompatibleOperation(httpMethod, issues))
  }

  private def checkParameter(writerParameter: Parameter, readerParameter: Parameter): Option[IncompatibleParameter] = {
    val isCompatibleStyle = readerParameter.style == writerParameter.style
    val isCompatibleExplode = readerParameter.explode == writerParameter.explode
    val isCompatibleAllowEmptyValue = readerParameter.allowEmptyValue == writerParameter.allowEmptyValue
    val isCompatibleAllowReserved = readerParameter.allowReserved == writerParameter.allowReserved

    val issues =
      checkSchema(writerParameter.schema, readerParameter.schema).toList ++
        checkContent(writerParameter.content, readerParameter.content).toList ++
        (if (!isCompatibleStyle) Some(MissMatch("style")) else None).toList ++
        (if (!isCompatibleExplode) Some(MissMatch("explode")) else None).toList ++
        (if (!isCompatibleAllowEmptyValue) Some(MissMatch("allowEmptyValue")) else None).toList ++
        (if (!isCompatibleAllowReserved) Some(MissMatch("allowReserved")) else None).toList

    if (issues.isEmpty)
      None
    else
      Some(IncompatibleParameter(writerParameter.name, issues))
  }

  private def checkContent(
      writerContent: ListMap[String, MediaType],
      readerContent: ListMap[String, MediaType]
  ): Option[IncompatibleContent] = {
    val issues = writerContent.flatMap { case (writerMediaType, writerMediaTypeDescription) =>
      val readerMediaTypeDescription = readerContent.get(writerMediaType)
      readerMediaTypeDescription match {
        case None => Some(MissingMediaType(writerMediaType))
        case Some(readerMediaTypeDescription) =>
          checkMediaType(writerMediaType, writerMediaTypeDescription, readerMediaTypeDescription)
      }
    }

    if (issues.isEmpty)
      None
    else
      Some(IncompatibleContent(issues.toList))
  }

  private def checkMediaType(
      mediaType: String,
      writerMediaTypeDescription: MediaType,
      readerMediaTypeDescription: MediaType
  ): Option[IncompatibleMediaType] = {
    val issues = checkSchema(writerMediaTypeDescription.schema, readerMediaTypeDescription.schema)
    if (issues.nonEmpty)
      Some(IncompatibleMediaType(mediaType, issues.toList))
    else
      None
    // TODO: encoding?
  }

  private def checkSchema(
      writerSchema: Option[SchemaLike],
      readerSchema: Option[SchemaLike]
  ): Option[OpenAPICompatibilityIssue] = {
    (readerSchema, writerSchema) match {
      case (Some(readerSchema: Schema), Some(writerSchema: Schema)) =>
        val readerSchemas = Map("readerSchema" -> readerSchema)
        val writerSchemas = Map("writerSchema" -> writerSchema)

        val schemaComparator = new SchemaComparator(readerSchemas, writerSchemas)
        val schemaIssues = schemaComparator.compare(readerSchema, writerSchema)
        if (schemaIssues.nonEmpty)
          Some(IncompatibleSchema(schemaIssues))
        else
          None
      case _ => None

    }
  }

  private def getOperationParameters(operation: Operation): List[Parameter] = {
    operation.parameters.flatMap {
      case Right(parameter) => Some(parameter)
      case Left(reference)  => resolveParameterReference(readerOpenAPI, reference.$ref)
    }
  }

  private def resolveParameterReference(openAPI: OpenAPI, ref: String): Option[Parameter] = {
    openAPI.components match {
      case Some(component) => component.getLocalParameter(ref)
      case None            => None
    }
  }

  private def checkRequestBody(
      writerRequestBody: RequestBody,
      readerRequestBody: RequestBody
  ): Option[IncompatibleRequestBody] = {
    val contentIssues = checkContent(readerRequestBody.content, writerRequestBody.content).toList
    if (contentIssues.nonEmpty)
      Some(IncompatibleRequestBody(contentIssues))
    else
      None
  }

  private def checkResponse(writerResponse: Response, readerResponse: Response): Option[IncompatibleResponse] = {
    val contentIssue = checkContent(readerResponse.content, writerResponse.content)
    val headerIssues = writerResponse.headers.flatMap {
      case (writerHeaderName, Right(writerHeader)) =>
        val readerHeader = readerResponse.headers.get(writerHeaderName)
        readerHeader match {
          case Some(Right(readerHeader)) => checkHeader(writerHeaderName, writerHeader, readerHeader)
          case None                      => Some(MissingHeader(writerHeaderName))
          case _                         => None
        }
      case _ => None
    }

    val issues = contentIssue.toList ++ headerIssues
    if (issues.nonEmpty)
      Some(IncompatibleResponse(issues))
    else
      None
  }

  private def checkHeader(
      headerName: String,
      writerHeader: Header,
      readerHeader: Header
  ): Option[IncompatibleHeader] = {
    val schemaIssues = checkSchema(writerHeader.schema, readerHeader.schema)
    val contentIssue = checkContent(readerHeader.content, writerHeader.content)
    val isCompatibleStyle = readerHeader.style == writerHeader.style
    val isCompatibleExplode = readerHeader.explode == writerHeader.explode
    val isCompatibleAllowEmptyValue = readerHeader.allowEmptyValue == writerHeader.allowEmptyValue
    val isCompatibleAllowReserved = readerHeader.allowReserved == writerHeader.allowReserved

    val issues =
      schemaIssues.toList ++
        contentIssue.toList ++
        (if (!isCompatibleStyle) Some(MissMatch("style")) else None).toList ++
        (if (!isCompatibleExplode) Some(MissMatch("explode")) else None).toList ++
        (if (!isCompatibleAllowEmptyValue) Some(MissMatch("allowEmptyValue")) else None).toList ++
        (if (!isCompatibleAllowReserved) Some(MissMatch("allowReserved")) else None).toList

    if (issues.nonEmpty)
      Some(IncompatibleHeader(headerName, issues))
    else
      None
  }
}
