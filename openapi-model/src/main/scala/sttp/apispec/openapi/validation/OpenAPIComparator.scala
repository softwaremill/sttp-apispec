package sttp.apispec.openapi.validation

import sttp.apispec.{Schema, SchemaLike}
import sttp.apispec.openapi.{
  Encoding,
  Header,
  MediaType,
  OpenAPI,
  Operation,
  Parameter,
  PathItem,
  RequestBody,
  Response
}
import sttp.apispec.validation.SchemaComparator

import scala.collection.immutable.ListMap

object OpenAPIComparator {
  def apply(clientOpenAPI: OpenAPI, serverOpenAPI: OpenAPI): OpenAPIComparator =
    new OpenAPIComparator(clientOpenAPI, serverOpenAPI)
}

/** A utility for comparing two OpenAPI specifications to validate their compatibility.
  *
  * The `OpenAPIComparator` class compares the client's OpenAPI specification with the server's specification to detect
  * and highlight compatibility issues. It evaluates various components including paths, operations, parameters, request
  * bodies, responses, headers, schemas, content, and media types.
  *
  * Note: This comparator does not compare meta-data, such as the info object, server lists, or descriptions in
  * properties.
  *
  * @param clientOpenAPI
  *   the OpenAPI specification provided by the client.
  * @param serverOpenAPI
  *   the OpenAPI specification provided by the server.
  */

class OpenAPIComparator private (
    clientOpenAPI: OpenAPI,
    serverOpenAPI: OpenAPI
) {

  private val httpMethods = List(
    ("get", (_: PathItem).get),
    ("post", (_: PathItem).post),
    ("patch", (_: PathItem).patch),
    ("delete", (_: PathItem).delete),
    ("options", (_: PathItem).options),
    ("trace", (_: PathItem).trace),
    ("head", (_: PathItem).head),
    ("put", (_: PathItem).put)
  )

  private val clientSchemas: Map[String, Schema] = clientOpenAPI.components match {
    case Some(components) =>
      components.schemas.flatMap {
        case (key, schema: Schema) => Some((key, schema))
        case _                     => None
      }
    case _ => Map.empty[String, Schema]
  }

  private val serverSchemas: Map[String, Schema] = serverOpenAPI.components match {
    case Some(components) =>
      components.schemas.flatMap {
        case (key, schema: Schema) => Some((key, schema))
        case _                     => None
      }
    case _ => Map.empty[String, Schema]
  }

  /** Compares the client and server OpenAPI specifications for compatibility.
    *
    * This method compares the paths in both specifications to identify compatibility issues. It detects
    * incompatibilities such as missing paths, parameter mismatches, schema inconsistencies, and other discrepancies. It
    * organizes the issues into a tree-like structure, with main issues and their sub-issues.
    *
    * @return
    *   a list of `OpenAPICompatibilityIssue` instances detailing the identified issues.
    */

  def compare(): List[OpenAPICompatibilityIssue] = {
    clientOpenAPI.paths.pathItems.toList.flatMap {
      case (pathName, clientPathItem) =>
        val serverPathItem = serverOpenAPI.paths.pathItems.get(pathName)
        serverPathItem match {
          case None => Some(MissingPath(pathName))
          case Some(serverPathItem) =>
            val pathIssues = checkPath(pathName, clientPathItem, serverPathItem)
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
      clientPathItem: PathItem,
      serverPathItem: PathItem
  ): Option[IncompatiblePath] = {
    val issues = httpMethods.flatMap { case (httpMethod, getOperation) =>
      val clientOperation = getOperation(clientPathItem)
      val serverOperation = getOperation(serverPathItem)

      (clientOperation, serverOperation) match {
        case (Some(_), None)                  => Some(MissingOperation(httpMethod))
        case (Some(clientOp), Some(serverOp)) => checkOperation(httpMethod, clientOp, serverOp)
        case _                                => None
      }
    }
    if (issues.isEmpty)
      None
    else
      Some(IncompatiblePath(pathName, issues))
  }

  private def checkOperation(
      httpMethod: String,
      clientOperation: Operation,
      serverOperation: Operation
  ): Option[IncompatibleOperation] = {
    val serverParameters = getOperationParameters(serverOperation)
    val clientParameters = getOperationParameters(clientOperation)

    val parametersIssue = clientParameters.flatMap { clientParameter =>
      val serverParameter = serverParameters.find(_.name == clientParameter.name)
      serverParameter match {
        case None                  => Some(MissingParameter(clientParameter.name))
        case Some(serverParameter) => checkParameter(clientParameter, serverParameter)
      }
    }

    val requestBodyIssue = (clientOperation.requestBody, serverOperation.requestBody) match {
      case (Some(Right(clientRequestBody)), Some(Right(serverRequestBody))) =>
        checkRequestBody(clientRequestBody, serverRequestBody)
      case (Some(Right(_)), None) => Some(MissingRequestBody())
      case _                      => None
    }

    val responsesIssues = clientOperation.responses.responses.flatMap {
      case (clientResponseKey, Right(clientResponse)) =>
        val serverResponse = serverOperation.responses.responses.get(clientResponseKey)
        serverResponse match {
          case Some(Right(serverResponse)) => checkResponse(clientResponse, serverResponse)
          case None                        => Some(MissingResponse(clientResponseKey))
          case _                           => None
        }
      case _ => None
    }

    // TODO: callbacks
    val incompatibleSecurityRequirements = clientOperation.security.filterNot(serverOperation.security.contains)
    val securityIssues = incompatibleSecurityRequirements.map(IncompatibleSecurityRequirement(_))

    val issues = parametersIssue ++ requestBodyIssue ++ responsesIssues ++ securityIssues
    if (issues.isEmpty)
      None
    else
      Some(IncompatibleOperation(httpMethod, issues))
  }

  private def checkParameter(clientParameter: Parameter, serverParameter: Parameter): Option[IncompatibleParameter] = {
    val isCompatibleStyle = serverParameter.style == clientParameter.style
    val isCompatibleExplode = serverParameter.explode == clientParameter.explode
    val isCompatibleAllowEmptyValue = serverParameter.allowEmptyValue == clientParameter.allowEmptyValue
    val isCompatibleAllowReserved = serverParameter.allowReserved == clientParameter.allowReserved
    val isCompatibleRequiredValue = serverParameter.required == clientParameter.required

    val issues =
      checkSchema(clientParameter.schema, serverParameter.schema).toList ++
        checkContent(clientParameter.content, serverParameter.content).toList ++
        (if (!isCompatibleStyle) Some(IncompatibleStyle(clientParameter.style, serverParameter.style))
         else None).toList ++
        (if (!isCompatibleExplode) Some(IncompatibleExplode(clientParameter.explode, serverParameter.explode))
         else None).toList ++
        (if (!isCompatibleAllowEmptyValue)
           Some(IncompatibleAllowEmptyValue(clientParameter.allowEmptyValue, serverParameter.allowEmptyValue))
         else None).toList ++
        (if (!isCompatibleAllowReserved)
           Some(IncompatibleAllowReserved(clientParameter.allowReserved, serverParameter.allowReserved))
         else None).toList ++
        (if (!isCompatibleRequiredValue)
           Some(IncompatibleRequiredValue(clientParameter.required, serverParameter.required))
         else None).toList

    if (issues.isEmpty)
      None
    else
      Some(IncompatibleParameter(clientParameter.name, issues))
  }

  private def checkContent(
      clientContent: ListMap[String, MediaType],
      serverContent: ListMap[String, MediaType]
  ): Option[IncompatibleContent] = {
    val issues = clientContent.flatMap { case (clientMediaType, clientMediaTypeDescription) =>
      val serverMediaTypeDescription = serverContent.get(clientMediaType)
      serverMediaTypeDescription match {
        case None => Some(MissingMediaType(clientMediaType))
        case Some(serverMediaTypeDescription) =>
          checkMediaType(clientMediaType, clientMediaTypeDescription, serverMediaTypeDescription)
      }
    }

    if (issues.isEmpty)
      None
    else
      Some(IncompatibleContent(issues.toList))
  }

  private def checkEncoding(
      encodingName: String,
      clientEncoding: Encoding,
      serverEncoding: Encoding
  ): Option[IncompatibleEncoding] = {
    val isCompatibleStyle = serverEncoding.style == clientEncoding.style
    val isCompatibleExplode = serverEncoding.explode == clientEncoding.explode
    val isCompatibleAllowReserved = serverEncoding.allowReserved == clientEncoding.allowReserved
    val isCompatibleContentType = serverEncoding.contentType == clientEncoding.contentType
    val headerIssues = clientEncoding.headers.flatMap {
      case (clientHeaderName, Right(clientHeader)) =>
        val serverHeader = serverEncoding.headers.get(clientHeaderName)
        serverHeader match {
          case Some(Right(serverHeader)) => checkResponseHeader(clientHeaderName, clientHeader, serverHeader)
          case None                      => Some(MissingHeader(clientHeaderName))
          case _                         => None
        }
      case _ => None
    }

    val issues = headerIssues ++
      (if (!isCompatibleStyle) Some(IncompatibleStyle(clientEncoding.style, serverEncoding.style)) else None).toList ++
      (if (!isCompatibleContentType)
         Some(IncompatibleContentType(clientEncoding.contentType, serverEncoding.contentType))
       else None).toList ++
      (if (!isCompatibleExplode) Some(IncompatibleExplode(clientEncoding.explode, serverEncoding.explode))
       else None).toList ++
      (if (!isCompatibleAllowReserved)
         Some(IncompatibleAllowReserved(clientEncoding.allowReserved, serverEncoding.allowReserved))
       else None).toList

    if (issues.nonEmpty)
      Some(IncompatibleEncoding(encodingName, issues.toList))
    else
      None
  }

  private def checkMediaType(
      mediaType: String,
      clientMediaTypeDescription: MediaType,
      serverMediaTypeDescription: MediaType
  ): Option[IncompatibleMediaType] = {
    val encodingIssues = clientMediaTypeDescription.encoding.flatMap { case (clientEncodingName, clientEncoding) =>
      val serverEncoding = serverMediaTypeDescription.encoding.get(clientEncodingName)
      serverEncoding match {
        case None                 => Some(MissingEncoding(clientEncodingName))
        case Some(serverEncoding) => checkEncoding(clientEncodingName, clientEncoding, serverEncoding)
      }
    }

    val issues = checkSchema(clientMediaTypeDescription.schema, serverMediaTypeDescription.schema) ++ encodingIssues
    if (issues.nonEmpty)
      Some(IncompatibleMediaType(mediaType, issues.toList))
    else
      None
  }

  private def checkSchema(
      clientSchema: Option[SchemaLike],
      serverSchema: Option[SchemaLike]
  ): Option[OpenAPICompatibilityIssue] = {
    (clientSchema, serverSchema) match {
      case (Some(clientSchema), Some(serverSchema)) =>
        val schemaComparator = new SchemaComparator(clientSchemas, serverSchemas)
        val schemaIssues = schemaComparator.compare(clientSchema, serverSchema)
        if (schemaIssues.nonEmpty)
          Some(IncompatibleSchema(schemaIssues))
        else
          None
      case (Some(_), None) => Some(MissingSchema())
      case _               => None
    }
  }

  private def getOperationParameters(operation: Operation): List[Parameter] = {
    operation.parameters.flatMap {
      case Right(parameter) => Some(parameter)
      case Left(reference)  => resolveParameterReference(serverOpenAPI, reference.$ref)
    }
  }

  private def resolveParameterReference(openAPI: OpenAPI, ref: String): Option[Parameter] = {
    openAPI.components match {
      case Some(component) => component.getLocalParameter(ref)
      case None            => None
    }
  }

  private def checkRequestBody(
      clientRequestBody: RequestBody,
      serverRequestBody: RequestBody
  ): Option[IncompatibleRequestBody] = {
    val isCompatibleRequiredValue = serverRequestBody.required == clientRequestBody.required
    val contentIssues = checkContent(clientRequestBody.content, serverRequestBody.content).toList

    val issues = contentIssues ++
      (if (!isCompatibleRequiredValue)
         Some(IncompatibleRequiredValue(clientRequestBody.required, serverRequestBody.required))
       else None).toList

    if (issues.nonEmpty)
      Some(IncompatibleRequestBody(issues))
    else
      None
  }

  private def checkResponse(clientResponse: Response, serverResponse: Response): Option[IncompatibleResponse] = {
    val contentIssue = checkContent(clientResponse.content, serverResponse.content)
    val headerIssues = clientResponse.headers.flatMap {
      case (clientHeaderName, Right(clientHeader)) =>
        val serverHeader = serverResponse.headers.get(clientHeaderName)
        serverHeader match {
          case Some(Right(serverHeader)) => checkResponseHeader(clientHeaderName, clientHeader, serverHeader)
          case None                      => Some(MissingHeader(clientHeaderName))
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

  private def checkResponseHeader(
      headerName: String,
      clientHeader: Header,
      serverHeader: Header
  ): Option[IncompatibleHeader] = {
    val schemaIssues = checkSchema(clientHeader.schema, serverHeader.schema)
    val contentIssue = checkContent(clientHeader.content, serverHeader.content)
    val isCompatibleStyle = serverHeader.style == clientHeader.style
    val isCompatibleExplode = serverHeader.explode == clientHeader.explode
    val isCompatibleAllowEmptyValue = serverHeader.allowEmptyValue == clientHeader.allowEmptyValue
    val isCompatibleAllowReserved = serverHeader.allowReserved == clientHeader.allowReserved
    val isCompatibleRequiredValue = serverHeader.required == clientHeader.required

    val issues =
      schemaIssues.toList ++
        contentIssue.toList ++
        (if (!isCompatibleStyle) Some(IncompatibleStyle(clientHeader.style, serverHeader.style)) else None).toList ++
        (if (!isCompatibleExplode) Some(IncompatibleExplode(clientHeader.explode, serverHeader.explode))
         else None).toList ++
        (if (!isCompatibleAllowEmptyValue)
           Some(IncompatibleAllowEmptyValue(clientHeader.allowEmptyValue, serverHeader.allowEmptyValue))
         else None).toList ++
        (if (!isCompatibleAllowReserved)
           Some(IncompatibleAllowReserved(clientHeader.allowReserved, serverHeader.allowReserved))
         else None).toList ++
        (if (!isCompatibleRequiredValue)
           Some(IncompatibleRequiredValue(clientHeader.required, serverHeader.required))
         else None).toList

    if (issues.nonEmpty)
      Some(IncompatibleHeader(headerName, issues))
    else
      None
  }
}
