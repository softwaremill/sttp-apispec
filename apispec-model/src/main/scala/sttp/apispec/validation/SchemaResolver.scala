package sttp.apispec.validation

import sttp.apispec.{AnySchema, Schema, SchemaLike}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

class SchemaResolver(schemas: Map[String, Schema]) {

  def discriminatorMapping(schema: Schema): ListMap[String, Schema] = {
    // schema reference -> overridden discriminator value
    val explicitDiscValueByRef = schema.discriminator.flatMap(_.mapping).getOrElse(ListMap.empty).map(_.swap)
    // assuming that schema is valid and every reference in disc.mapping is also an element of oneOf/anyOf
    ListMap.empty ++ (schema.oneOf ++ schema.anyOf).collect { case s @ ReferenceSchema(ref) =>
      val discValue = explicitDiscValueByRef.getOrElse(
        ref,
        ref match {
          case SchemaResolver.Reference(name) => name
          case _ => throw new NoSuchElementException(s"no discriminator value specified for non-local reference $ref")
        }
      )
      discValue -> s
    }
  }

  @tailrec final def resolveAndNormalize(schema: SchemaLike): Schema = schema match {
    case AnySchema.Anything => Schema.Empty
    case AnySchema.Nothing  => Schema.Nothing
    case s @ ReferenceSchema(SchemaResolver.Reference(name)) =>
      resolveAndNormalize(
        schemas.getOrElse(name, throw new NoSuchElementException(s"could not resolve schema reference ${s.$ref.get}"))
      )
    case s: Schema => normalize(s)
  }

  private object ReferenceSchema {
    def unapply(schema: Schema): Option[String] =
      schema.$ref.filter(ref => schema == Schema($ref = Some(ref)))
  }

  private def normalize(schema: Schema): Schema =
    schema.copy(
      $comment = None,
      $defs = None,
      $schema = None,
      title = None,
      description = None,
      default = None,
      deprecated = None,
      readOnly = None,
      writeOnly = None,
      examples = None,
      externalDocs = None,
      extensions = ListMap.empty
    )
}

object SchemaResolver {
  val ComponentsRefPrefix = "#/components/schemas/"

  val DefsRefPrefix = "#/$defs/"

  private val Reference = new References(ComponentsRefPrefix, DefsRefPrefix)

  private class References(prefix: String*) {
    def unapply(ref: String): Option[String] = prefix.flatMap { p =>
      Option(ref).filter(_.startsWith(p)).map(_.stripPrefix(p))
    }.headOption
  }

  def apply(schemas: Map[String, Schema]): SchemaResolver = new SchemaResolver(schemas)

  def apply(schema: Schema): SchemaResolver = new SchemaResolver(
    schema.$defs.getOrElse(Map.empty).collect { case (name, s: Schema) => name -> s }
  )
}
