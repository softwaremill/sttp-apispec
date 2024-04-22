package sttp.apispec.validation

import sttp.apispec._

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

object SchemaComparator {
  final val RefPrefix = "#/components/schemas/"

  def compare(
    writerSchema: Schema,
    readerSchema: Schema,
    writerNamedSchemas: Map[String, Schema] = Map.empty,
    readerNamedSchemas: Map[String, Schema] = Map.empty
  ): List[SchemaCompatibilityIssue] =
    new SchemaComparator(writerNamedSchemas, readerNamedSchemas)
      .compare(writerSchema, readerSchema)
}

private class SchemaComparator(
  writerNamedSchemas: Map[String, Schema],
  readerNamedSchemas: Map[String, Schema]
) {

  import SchemaComparator._

  private val cache = new mutable.HashMap[(Schema, Schema), List[SchemaCompatibilityIssue]]

  // keeps schema pairs for which comparison is ongoing in order to short circuit recursive schema comparisons
  private val inComparison = new mutable.HashSet[(Schema, Schema)]

  private def compare(writerSchema: SchemaLike, readerSchema: SchemaLike): List[SchemaCompatibilityIssue] = {
    val normalizedWriterSchema = normalize(writerSchema, writerNamedSchemas)
    val normalizedReaderSchema = normalize(readerSchema, readerNamedSchemas)
    val cacheKey = (normalizedWriterSchema, normalizedReaderSchema)
    // short-circuit recursive comparison of the same pair of schemas that is already being compared
    if (inComparison(cacheKey)) Nil
    else cache.getOrElseUpdate(cacheKey, {
      inComparison.add(cacheKey)
      try compareNormalized(normalizedWriterSchema, normalizedReaderSchema)
      finally inComparison.remove(cacheKey)
    })
  }

  // translate AnySchema to Schema, remove annotations and resolve references
  @tailrec private def normalize(schema: SchemaLike, named: Map[String, Schema]): Schema = schema match {
    case AnySchema.Anything => Schema.Empty
    case AnySchema.Nothing => Schema.Nothing
    case s: Schema => deannotate(s) match {
      case s@LocalReference(name) =>
        def noSchema: Nothing =
          throw new NoSuchElementException(s"could not resolve schema reference ${s.$ref.get}")
        //TODO: detect cycles and handle recursive schema definitions
        normalize(named.getOrElse(name, noSchema), named)
      case s => s
    }
  }

  /** Matches a schema that is a pure reference to one of the component schemas */
  private object LocalReference {
    def unapply(schema: Schema): Option[String] =
      schema.$ref.filter(_.startsWith(RefPrefix))
        .map(_.stripPrefix(RefPrefix))
        .filter(name => schema == Schema.referenceTo(RefPrefix, name))
  }

  // strip annotations so that schemas can be compared structurally for equivalence
  private def deannotate(schema: Schema): Schema =
    schema.copy(
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

  private def compareNormalized(writerSchema: Schema, readerSchema: Schema): List[SchemaCompatibilityIssue] =
    if (writerSchema == Schema.Nothing || readerSchema == Schema.Empty || writerSchema == readerSchema) {
      Nil
    } else if (isPrimitiveSchema(writerSchema) && isPrimitiveSchema(readerSchema)) {
      checkType(writerSchema, readerSchema).toList ++
        checkEnumAndConst(writerSchema, readerSchema).toList ++
        checkFormat(writerSchema, readerSchema).toList ++
        checkMultipleOf(writerSchema, readerSchema).toList ++
        checkNumericBounds(writerSchema, readerSchema).toList ++
        checkStringLengthBounds(writerSchema, readerSchema).toList ++
        checkPattern(writerSchema, readerSchema).toList

    } else if (isNullableSchema(readerSchema)) {
      val ws = if (isNullableSchema(writerSchema)) writerSchema.anyOf.head else writerSchema
      compare(ws, readerSchema.anyOf.head)

    } else if (isProductSchema(writerSchema) && isProductSchema(readerSchema)) {
      // Even though the default value for `additionalProperties` is an empty schema that accepts everything,
      // we assume a Nothing schema for the writer because it's extremely unlikely in practice that a value for a
      // product type would contain an additional property beyond what's listed in `properties`
      val writerAdditionalProps = writerSchema.additionalProperties.getOrElse(Schema.Nothing)
      val readerAdditionalProps = readerSchema.additionalProperties.getOrElse(Schema.Empty)
      val allPropNames = writerSchema.properties.keySet ++ readerSchema.properties.keySet
      val propIssues = allPropNames.toList.flatMap { propName =>
        val writerPropSchema = writerSchema.properties.getOrElse(propName, writerAdditionalProps)
        val readerPropSchema = readerSchema.properties.getOrElse(propName, readerAdditionalProps)
        val issues = compare(writerPropSchema, readerPropSchema)
        if (issues.nonEmpty) Some(IncompatibleProperty(propName, issues)) else None
      }

      checkType(writerSchema, readerSchema).toList ++
        checkRequiredProperties(writerSchema, readerSchema).toList ++
        checkDependentRequired(writerSchema, readerSchema) ++
        propIssues ++
        checkPropertyNames(writerSchema, readerSchema).toList ++
        checkAdditionalProperties(writerSchema, readerSchema).toList ++
        checkMinMaxProperties(writerSchema, readerSchema).toList

    } else if (isCoproductSchema(writerSchema) && isCoproductSchema(readerSchema) &&
      // if readerSchema does not have a discriminator, we fall back to GeneralSchemaMismatch
      // TODO: support comparison of untagged unions
      readerSchema.discriminator.nonEmpty
    ) {
      val writerMapping = discriminatorMapping(writerSchema)
      val readerMapping = discriminatorMapping(readerSchema)

      val variantIssues: List[SchemaCompatibilityIssue] = (writerMapping, readerMapping) match {
        case (Some(wm), Some(rm)) =>
          (wm.keySet intersect rm.keySet).toList.flatMap { tag =>
            compare(wm(tag), rm(tag)) match {
              case Nil => None
              case issues => Some(IncompatibleDiscriminatorCase(tag, issues))
            }
          }
        case _ => Nil
      }

      checkType(writerSchema, readerSchema).toList ++
        checkDiscriminatorProp(writerSchema, readerSchema).toList ++
        checkDiscriminatorValues(writerMapping, readerMapping).toList ++
        variantIssues

    } else if (isCollectionSchema(writerSchema) && isCollectionSchema(readerSchema)) {
      checkType(writerSchema, readerSchema).toList ++
        checkArrayLengthBounds(writerSchema, readerSchema).toList ++
        checkUniqueItems(writerSchema, readerSchema).toList ++
        checkItems(writerSchema, readerSchema).toList

    } else if (isTupleSchema(writerSchema) && isTupleSchema(readerSchema)) {
      val writerPrefixItems = writerSchema.prefixItems.getOrElse(Nil)
      val readerPrefixItems = readerSchema.prefixItems.getOrElse(Nil)
      val prefixItemsIssues = writerPrefixItems
        .zipAll(readerPrefixItems, Schema.Empty, Schema.Empty)
        .zipWithIndex.flatMap {
          case ((writerItem, readerItem), idx) =>
            compare(writerItem, readerItem) match {
              case Nil => None
              case issues => Some(IncompatiblePrefixItem(idx, issues))
            }
        }

      checkType(writerSchema, readerSchema).toList ++
        checkArrayLengthBounds(writerSchema, readerSchema).toList ++
        prefixItemsIssues

    } else if (isMapSchema(writerSchema) && isMapSchema(readerSchema)) {
      checkAdditionalProperties(writerSchema, readerSchema).toList ++
        checkPropertyNames(writerSchema, readerSchema).toList ++
        checkType(writerSchema, readerSchema).toList ++
        checkMinMaxProperties(writerSchema, readerSchema).toList

    } else if (readerSchema == Schema.Nothing) {
      List(NoValuesAllowed(writerSchema))

    } else checkType(writerSchema, readerSchema) match {
      case Some(typeMismatch) => List(typeMismatch)
      case None =>
        // At this point we know that schemas are not equal, and we were unable to produce any
        // more specific incompatibility, so we just return a generic issue
        List(GeneralSchemaMismatch(writerSchema, readerSchema))
    }

  /**
   * Matches a schema that uses `anyOf` to make another, base schema nullable.
   * Most of the time, the base schema is a reference.
   */
  private def isNullableSchema(s: Schema): Boolean =
    s == Schema(anyOf = s.anyOf) && (s.anyOf match {
      case List(_, Schema.Null) => true
      case _ => false
    })

  /** Checks if schema is for a _primitive_ value, i.e. a string, boolean, number or null */
  private def isPrimitiveSchema(s: Schema): Boolean =
    s.`type`.exists { types =>
      types.nonEmpty && !types.contains(SchemaType.Array) && !types.contains(SchemaType.Object)
    } && s == Schema( // check if the schema contains only primitive type assertions
      `type` = s.`type`,
      enum = s.`enum`,
      const = s.`const`,
      format = s.format,

      multipleOf = s.multipleOf,
      minimum = s.minimum,
      exclusiveMinimum = s.exclusiveMinimum,
      maximum = s.maximum,
      exclusiveMaximum = s.exclusiveMaximum,

      maxLength = s.maxLength,
      minLength = s.minLength,
      pattern = s.pattern,
    )

  private def isCollectionSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Array) &&
      s.items.isDefined &&
      s == Schema(
        `type` = s.`type`,
        items = s.items,
        maxItems = s.maxItems,
        minItems = s.minItems,
        uniqueItems = s.uniqueItems,
      )

  private def isTupleSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Array) &&
      s.prefixItems.isDefined &&
      s == Schema(
        `type` = s.`type`,
        items = s.items,
        prefixItems = s.prefixItems,
        maxItems = s.maxItems,
        minItems = s.minItems,
      )

  private def isMapSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Object) &&
      s.additionalProperties.isDefined &&
      s == Schema(
        `type` = s.`type`,
        propertyNames = s.propertyNames,
        additionalProperties = s.additionalProperties,
        maxProperties = s.maxProperties,
        minProperties = s.minProperties,
      )

  private def isProductSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Object) &&
      s.properties.nonEmpty &&
      s == Schema(
        `type` = s.`type`,
        propertyNames = s.propertyNames,
        properties = s.properties,
        required = s.required,
        dependentRequired = s.dependentRequired,
        additionalProperties = s.additionalProperties,
        maxProperties = s.maxProperties,
        minProperties = s.minProperties
      )

  // coproduct schema is a schema with `oneOf` or `anyOf` of pure references, with an optional discriminator object
  private def isCoproductSchema(s: Schema): Boolean =
    // exactly one of `oneOf` and `anyOf` should be non-empty
    (s.oneOf.nonEmpty != s.anyOf.nonEmpty) &&
      (s.oneOf ++ s.anyOf).forall {
        case LocalReference(_) => true
        case _ => false
      } && s == Schema(
      oneOf = s.oneOf,
      anyOf = s.anyOf,
      discriminator = s.discriminator,
    )

  private def discriminatorMapping(schema: Schema): Option[ListMap[String, SchemaLike]] =
    schema.discriminator.map { disc =>
      val baseMapping = schema.oneOf.collect({ case s@LocalReference(name) => name -> s }).to(ListMap)
      baseMapping ++ disc.mapping.getOrElse(ListMap.empty).map {
        case (name, ref) => name -> Schema($ref = Some(ref))
      }
    }

  /**
   * Checks if all writer types are compatible with at least one reader type.
   * This check assumes schemas that have at least one `type` defined.
   */
  private def checkType(writerSchema: Schema, readerSchema: Schema): Option[TypeMismatch] =
    for {
      writerTypes <- writerSchema.`type`
      readerTypes <- readerSchema.`type`
      incompatibleWriterTypes =
        writerTypes.filter(wtpe => !readerTypes.exists(rtpe => typesCompatible(wtpe, rtpe)))
      if incompatibleWriterTypes.nonEmpty
    } yield TypeMismatch(incompatibleWriterTypes, readerTypes)

  private def typesCompatible(writerTpe: SchemaType, readerTpe: SchemaType): Boolean =
    (writerTpe, readerTpe) match {
      case (SchemaType.Integer, SchemaType.Number) => true
      case _ => writerTpe == readerTpe
    }

  private def checkEnumAndConst(writerSchema: Schema, readerSchema: Schema): Option[EnumMismatch] = {
    val writerEnum = writerSchema.const.map(List(_)).orElse(writerSchema.`enum`)
    val readerEnum = readerSchema.const.map(List(_)).orElse(readerSchema.`enum`)
    (writerEnum, readerEnum) match {
      case (None, Some(readerValues)) =>
        Some(EnumMismatch(None, readerValues))
      case (Some(writerValues), Some(readerValues)) =>
        val incompatibleWriterValues =
          writerValues.filter(wv => !readerValues.contains(wv))
        if (incompatibleWriterValues.isEmpty) None
        else Some(EnumMismatch(Some(incompatibleWriterValues), readerValues))
      case _ =>
        None
    }
  }

  private def checkFormat(writerSchema: Schema, readerSchema: Schema): Option[FormatMismatch] =
    (writerSchema.format, readerSchema.format) match {
      case (None, Some(readerFormat)) =>
        Some(FormatMismatch(None, readerFormat))
      case (Some(writerFormat), Some(readerFormat)) if !formatsCompatible(writerFormat, readerFormat) =>
        Some(FormatMismatch(Some(writerFormat), readerFormat))
      case _ => None
    }

  private def formatsCompatible(writerFormat: String, readerFormat: String): Boolean =
    (writerFormat, readerFormat) match {
      case (SchemaFormat.Int32, SchemaFormat.Int64) => true
      case (SchemaFormat.Float | SchemaFormat.Int32, SchemaFormat.Double) => true
      case _ => writerFormat == readerFormat
    }

  private def checkMultipleOf(writerSchema: Schema, readerSchema: Schema): Option[MultipleOfMismatch] =
    (writerSchema.multipleOf, readerSchema.multipleOf) match {
      case (None, Some(readerMultiplier)) =>
        Some(MultipleOfMismatch(None, readerMultiplier))
      case (Some(writerMultiplier), Some(readerMultiplier)) if !(writerMultiplier / readerMultiplier).isWhole =>
        Some(MultipleOfMismatch(Some(writerMultiplier), readerMultiplier))
      case _ =>
        None
    }

  private def bounds(schema: Schema): Bounds[BigDecimal] = Bounds(
    schema.minimum
      .map(Bound.inclusive)
      .orElse(schema.exclusiveMinimum.map(Bound.exclusive)),
    schema.maximum
      .map(Bound.inclusive)
      .orElse(schema.exclusiveMaximum.map(Bound.exclusive))
  )

  private def checkNumericBounds(writerSchema: Schema, readerSchema: Schema): Option[BoundsMismatch] = {
    val writerBounds = bounds(writerSchema)
    val readerBounds = bounds(readerSchema)
    if (readerBounds.contains(writerBounds)) None
    else Some(BoundsMismatch(writerBounds, readerBounds))
  }

  private def stringLengthBounds(schema: Schema): Bounds[Int] = Bounds(
    Some(Bound.inclusive(schema.minLength.getOrElse(0))),
    schema.maxLength.map(Bound.inclusive)
  )

  private def checkStringLengthBounds(
    writerSchema: Schema,
    readerSchema: Schema
  ): Option[StringLengthBoundsMismatch] = {
    val writerBounds = stringLengthBounds(writerSchema)
    val readerBounds = stringLengthBounds(readerSchema)
    if (readerBounds.contains(writerBounds)) None
    else Some(StringLengthBoundsMismatch(writerBounds, readerBounds))
  }

  private def checkPattern(writerSchema: Schema, readerSchema: Schema): Option[PatternMismatch] =
    (writerSchema.pattern, readerSchema.pattern) match {
      case (None, Some(readerPattern)) =>
        Some(PatternMismatch(None, readerPattern))
      case (Some(writerPattern), Some(readerPattern)) if writerPattern != readerPattern =>
        Some(PatternMismatch(Some(writerPattern), readerPattern))
      case _ =>
        None
    }

  private def checkUniqueItems(writerSchema: Schema, readerSchema: Schema): Option[UniqueItemsIntroduced.type] =
    (writerSchema.uniqueItems, readerSchema.uniqueItems) match {
      case (None | Some(false), Some(true)) => Some(UniqueItemsIntroduced)
      case _ => None
    }

  private def arrayLengthBounds(schema: Schema): Bounds[Int] = Bounds(
    Some(Bound.inclusive(schema.minItems.getOrElse(0))),
    schema.maxItems.map(Bound.inclusive)
  )

  private def checkArrayLengthBounds(writerSchema: Schema, readerSchema: Schema): Option[ArrayLengthBoundsMismatch] = {
    val writerBounds = arrayLengthBounds(writerSchema)
    val readerBounds = stringLengthBounds(readerSchema)
    if (readerBounds.contains(writerBounds)) None
    else Some(ArrayLengthBoundsMismatch(writerBounds, readerBounds))
  }

  private def checkItems(writerSchema: Schema, readerSchema: Schema): Option[IncompatibleItems] = {
    val writerItems = writerSchema.items.getOrElse(Schema.Empty)
    val readerItems = readerSchema.items.getOrElse(Schema.Empty)
    compare(writerItems, readerItems) match {
      case Nil => None
      case issues => Some(IncompatibleItems(issues))
    }
  }

  private def objectMinMaxProperties(schema: Schema): Bounds[Int] = Bounds(
    Some(Bound.inclusive(schema.minProperties.getOrElse(0))),
    schema.maxProperties.map(Bound.inclusive)
  )

  private def checkMinMaxProperties(writerSchema: Schema, readerSchema: Schema): Option[MinMaxPropertiesMismatch] = {
    val writerBounds = objectMinMaxProperties(writerSchema)
    val readerBounds = objectMinMaxProperties(readerSchema)
    if (readerBounds.contains(writerBounds)) None
    else Some(MinMaxPropertiesMismatch(writerBounds, readerBounds))
  }

  private def checkRequiredProperties(writerSchema: Schema, readerSchema: Schema): Option[MoreRequiredProperties] = {
    val newRequired = readerSchema.required.toSet -- writerSchema.required.toSet
    if (newRequired.isEmpty) None
    else Some(MoreRequiredProperties(newRequired))
  }

  private def checkDependentRequired(writerSchema: Schema, readerSchema: Schema): List[MoreDependentRequired] =
    readerSchema.dependentRequired.toList.flatMap { case (property, required) =>
      // if the writer schema does not require or define a schema for this property, we assume that it will never
      // be passed (even if it's implicitly allowed by patternProperties/additionalProperties)
      val writerMentionsProperty =
        writerSchema.properties.contains(property) || writerSchema.required.contains(property)
      val writerRequired = writerSchema.dependentRequired
        .get(property)
        .orElse(if (writerMentionsProperty) Some(Nil) else None)

      writerRequired.flatMap { wr =>
        val moreRequired = required.toSet -- wr.toSet
        if (moreRequired.isEmpty) None
        else Some(MoreDependentRequired(property, moreRequired))
      }
    }

  private def checkDiscriminatorProp(
    writerSchema: Schema,
    readerSchema: Schema
  ): Option[DiscriminatorPropertyMismatch] = {
    val writerDiscProp = writerSchema.discriminator.map(_.propertyName)
    val readerDiscProp = readerSchema.discriminator.map(_.propertyName)
    (writerDiscProp, readerDiscProp) match {
      case (None, Some(readerProp)) =>
        Some(DiscriminatorPropertyMismatch(None, readerProp))
      case (Some(writerProp), Some(readerProp)) if writerProp != readerProp =>
        Some(DiscriminatorPropertyMismatch(writerDiscProp, readerProp))
      case _ =>
        None
    }
  }

  private def checkDiscriminatorValues(
    writerMapping: Option[ListMap[String, SchemaLike]],
    readerMapping: Option[ListMap[String, SchemaLike]],
  ): Option[DiscriminatorValuesMismatch] =
    for {
      wm <- writerMapping
      rm <- readerMapping
      unsupportedValues = wm.keySet -- rm.keySet
      if unsupportedValues.nonEmpty
    } yield DiscriminatorValuesMismatch(unsupportedValues.toList)

  private def checkPropertyNames(
    writerSchema: Schema,
    readerSchema: Schema,
  ): Option[IncompatiblePropertyNames] = {
    val writerPropertyNames = writerSchema.propertyNames.getOrElse(Schema.Empty)
    val readerPropertyNames = readerSchema.propertyNames.getOrElse(Schema.Empty)
    compare(writerPropertyNames, readerPropertyNames) match {
      case Nil => None
      case issues => Some(IncompatiblePropertyNames(issues))
    }
  }

  private def checkAdditionalProperties(
    writerSchema: Schema,
    readerSchema: Schema,
  ): Option[IncompatibleAdditionalProperties] = {
    val writerProps = writerSchema.additionalProperties.getOrElse(Schema.Empty)
    val readerProps = readerSchema.additionalProperties.getOrElse(Schema.Empty)
    compare(writerProps, readerProps) match {
      case Nil => None
      case issues => Some(IncompatibleAdditionalProperties(issues))
    }
  }

}