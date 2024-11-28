package sttp.apispec.openapi.validation

import sttp.apispec.{Schema, SchemaLike}
import sttp.apispec.openapi.{MediaType, OpenAPI, Operation, Parameter, PathItem}
import sttp.apispec.validation.SchemaComparator

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
        case (Some(readerOp), Some(writerOp)) => checkOperation(httpMethod, readerOp, writerOp)
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
      readerOperation: Operation,
      writerOperation: Operation
  ): Option[IncompatibleOperation] = {
    val readerParameters = getOperationParameters(readerOperation)
    val writerParameters = getOperationParameters(writerOperation)

    val issues = writerParameters.flatMap { writerParameter =>
      val readerParameter = readerParameters.find(_.name == writerParameter.name)
      readerParameter match {
        case None                  => Some(MissingParameter(writerParameter.name))
        case Some(readerParameter) => checkParameter(readerParameter, writerParameter)
      }
    }

    if (issues.isEmpty)
      None
    else
      Some(IncompatibleOperation(httpMethod, issues))
  }

  private def checkParameter(readerParameter: Parameter, writerParameter: Parameter): Option[IncompatibleParameter] = {
    val isCompatibleStyle = readerParameter.style == writerParameter.style
    val isCompatibleExplode = readerParameter.explode == writerParameter.explode
    val isCompatibleAllowEmptyValue = readerParameter.allowEmptyValue == writerParameter.allowEmptyValue
    val isCompatibleAllowReserved = readerParameter.allowReserved == writerParameter.allowReserved

    val issues =
      checkSchema(readerParameter.schema, writerParameter.schema).toList ++
        checkParameterContent(readerParameter, writerParameter).toList ++
        (if (!isCompatibleStyle) Some(MissMatch("style")) else None).toList ++
        (if (!isCompatibleExplode) Some(MissMatch("explode")) else None).toList ++
        (if (!isCompatibleAllowEmptyValue) Some(MissMatch("allowEmptyValue")) else None).toList ++
        (if (!isCompatibleAllowReserved) Some(MissMatch("allowReserved")) else None).toList

    if (issues.isEmpty)
      None
    else
      Some(IncompatibleParameter(writerParameter.name, issues))
  }

  private def checkParameterContent(
      readerParameter: Parameter,
      writerParameter: Parameter
  ): Option[IncompatibleParameterContent] = {
    val issues = writerParameter.content.flatMap { case (writerMediaType, writerMediaTypeDescription) =>
      val readerMediaTypeDescription = readerParameter.content.get(writerMediaType)
      readerMediaTypeDescription match {
        case None => Some(MissingMediaType(writerMediaType))
        case Some(readerMediaTypeDescription) =>
          checkMediaType(writerMediaType, writerMediaTypeDescription, readerMediaTypeDescription)
      }
    }

    if (issues.isEmpty)
      None
    else
      Some(IncompatibleParameterContent(issues.toList))
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
      readerSchema: Option[SchemaLike],
      writerSchema: Option[SchemaLike]
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
}
