package sttp.apispec.validation

import sttp.apispec._

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

/** Utility for comparing schemas for compatibility. See [[compare]] for more details.
  *
  * Since this class contains a cache of comparison results, it is meant to be reused between multiple schema
  * comparisons.
  *
  * @param writerSchemaResolver
  *   can resolve named schemas which may be referred to by the writer schema
  * @param readerSchemaResolver
  *   can resolve named schemas which may be referred to by the reader schema
  */
class SchemaComparator(
    writerSchemaResolver: SchemaResolver,
    readerSchemaResolver: SchemaResolver
) {

  def this(
      writerNamedSchemas: Map[String, Schema],
      readerNamedSchemas: Map[String, Schema]
  ) = this(SchemaResolver(writerNamedSchemas), SchemaResolver(readerNamedSchemas))

  private val issuesCache = new mutable.HashMap[(Schema, Schema), List[SchemaCompatibilityIssue]]
  private val identicalityCache = new mutable.HashMap[(Schema, Schema), Boolean]

  /** Computes a value for a given key, using a cache. Computation of the value may recursively depend on the same key.
    * In such cases, the recursion is short-circuited and the value is assumed to be equal to `recursiveValue`.
    */
  private def computeCached[K, V](cache: mutable.Map[K, V], key: K, recursiveValue: V)(compute: => V): V =
    cache.get(key) match {
      case Some(value) => value
      case None =>
        cache.put(key, recursiveValue)
        val result =
          try compute
          finally cache.remove(key)
        cache.put(key, result)
        result
    }

  /** Compares two schemas for compatibility. More precisely, checks if data that is valid according to [[writerSchema]]
    * is also valid according to [[readerSchema]]. If not, a list of compatibility issues is returned.
    *
    * Determining compatibility (or incompatibility) of arbitrary schemas with certainty is non-trivial, or outright
    * impossible in general. For this reason, this method works in a "best effort" manner, assuming that the schemas
    * match one of the typical schema patterns generated by libraries like `tapir`. In more complex situations, the
    * comparator simply falls back to comparing schemas by plain equality, or returns a [[GeneralSchemaMismatch]], which
    * indicates comparator's inability to definitely determine compatibility or incompatibility of the schemas.
    *
    * In practice, the comparator is designed to detect typical changes that may appear in schemas during API evolution,
    * e.g. adding new fields, changing types, etc.
    *
    * Before being compared, all schemas are stripped of keywords which do not affect the comparison, e.g. annotations
    * like `title`, `description`, etc.
    *
    * @param writerSchema
    *   schema of the data being written
    * @param readerSchema
    *   schema of the data being read
    * @return
    *   a list of incompatibilities between the schemas
    */
  def compare(writerSchema: SchemaLike, readerSchema: SchemaLike): List[SchemaCompatibilityIssue] = {
    val normalizedWriterSchema = writerSchemaResolver.resolveAndNormalize(writerSchema)
    val normalizedReaderSchema = readerSchemaResolver.resolveAndNormalize(readerSchema)
    computeCached(issuesCache, (normalizedWriterSchema, normalizedReaderSchema), Nil) {
      compareNormalized(normalizedWriterSchema, normalizedReaderSchema)
    }
  }

  private def compareNormalized(writerSchema: Schema, readerSchema: Schema): List[SchemaCompatibilityIssue] =
    if (writerSchema == Schema.Nothing || readerSchema == Schema.Empty) {
      Nil
    } else if (isPrimitiveSchema(writerSchema) && isPrimitiveSchema(readerSchema)) {
      checkType(writerSchema, readerSchema).toList ++
        checkEnumAndConst(writerSchema, readerSchema).toList ++
        checkFormat(writerSchema, readerSchema).toList ++
        checkMultipleOf(writerSchema, readerSchema).toList ++
        checkNumericBounds(writerSchema, readerSchema).toList ++
        checkStringLengthBounds(writerSchema, readerSchema).toList ++
        checkPattern(writerSchema, readerSchema).toList

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
        propIssues

    } else if (isDiscriminatedUnionSchema(writerSchema) && isDiscriminatedUnionSchema(readerSchema)) {
      val writerMapping = writerSchemaResolver.discriminatorMapping(writerSchema)
      val readerMapping = readerSchemaResolver.discriminatorMapping(readerSchema)

      val variantIssues: List[SchemaCompatibilityIssue] =
        (writerMapping.keySet intersect readerMapping.keySet).toList.flatMap { tag =>
          compare(writerMapping(tag), readerMapping(tag)) match {
            case Nil    => None
            case issues => Some(IncompatibleDiscriminatorCase(tag, issues))
          }
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
        .zipWithIndex
        .flatMap { case ((writerItem, readerItem), idx) =>
          compare(writerItem, readerItem) match {
            case Nil    => None
            case issues => Some(IncompatiblePrefixItem(idx, issues))
          }
        }

      checkType(writerSchema, readerSchema).toList ++
        checkArrayLengthBounds(writerSchema, readerSchema).toList ++
        prefixItemsIssues

    } else if (isMapSchema(writerSchema) && isMapSchema(readerSchema)) {
      checkType(writerSchema, readerSchema).toList ++
        checkAdditionalProperties(writerSchema, readerSchema).toList ++
        checkPropertyNames(writerSchema, readerSchema).toList ++
        checkMinMaxProperties(writerSchema, readerSchema).toList

    } else if (isUnionSchema(writerSchema)) {
      val variants = writerSchema.oneOf ++ writerSchema.anyOf
      variants.zipWithIndex.flatMap { case (variant, idx) =>
        compare(variant, readerSchema) match {
          case Nil    => None
          case issues => Some(IncompatibleUnionVariant(idx, issues))
        }
      }
    } else if (isUnionSchema(readerSchema)) {
      val variants = readerSchema.oneOf ++ readerSchema.anyOf

      @tailrec def alternatives(
          variants: List[SchemaLike],
          acc: List[List[SchemaCompatibilityIssue]]
      ): Option[AlternativeIssues] = variants match {
        case Nil => Some(AlternativeIssues(acc.reverse))
        case variant :: tail =>
          compare(writerSchema, variant) match {
            case Nil    => None
            case issues => alternatives(tail, issues :: acc)
          }
      }

      alternatives(variants, Nil).toList
    } else if (readerSchema == Schema.Nothing) {
      List(NoValuesAllowed(writerSchema))

    } else
      checkType(writerSchema, readerSchema) match {
        case Some(typeMismatch)                            => List(typeMismatch)
        case None if identical(writerSchema, readerSchema) => Nil
        case None                                          =>
          // At this point we know that schemas are not equal, and we were unable to produce any
          // more specific incompatibility, so we just return a generic issue
          List(GeneralSchemaMismatch(writerSchema, readerSchema))
      }

  /** Checks if two (already normalized) schemas are identical. Note: we can't simply compare schemas for equality with
    * `==` because local references in the schemas may resolve to different schemas, i.e.
    *   - identical local references in `writerSchema` and `readerSchema` may resolve to different schemas
    *   - non-identical local references in `writerSchema` and `readerSchema` may resolve to identical schemas
    */
  private def identical(writerSchema: Schema, readerSchema: Schema): Boolean =
    (writerSchema eq readerSchema) || computeCached(identicalityCache, (writerSchema, readerSchema), true) {
      def identicalSubschema(writerSubschema: SchemaLike, readerSubschema: SchemaLike): Boolean =
        identical(
          writerSchemaResolver.resolveAndNormalize(writerSubschema),
          readerSchemaResolver.resolveAndNormalize(readerSubschema)
        )

      def identicalSubschemaMap[K](
          writerSubschemas: ListMap[K, SchemaLike],
          readerSubschemas: ListMap[K, SchemaLike]
      ): Boolean =
        (writerSubschemas.keySet ++ readerSubschemas.keySet).forall { key =>
          writerSubschemas.get(key).toList.corresponds(readerSubschemas.get(key).toList)(identicalSubschema)
        }

      removeSubschemas(writerSchema) == removeSubschemas(readerSchema) &&
      writerSchema.$defs.toList.corresponds(readerSchema.$defs.toList)(identicalSubschemaMap) &&
      writerSchema.allOf.corresponds(readerSchema.allOf)(identicalSubschema) &&
      writerSchema.anyOf.corresponds(readerSchema.anyOf)(identicalSubschema) &&
      writerSchema.oneOf.corresponds(readerSchema.oneOf)(identicalSubschema) &&
      writerSchema.not.toList.corresponds(readerSchema.not.toList)(identicalSubschema) &&
      writerSchema.`if`.toList.corresponds(readerSchema.`if`.toList)(identicalSubschema) &&
      writerSchema.`then`.toList.corresponds(readerSchema.`then`.toList)(identicalSubschema) &&
      writerSchema.`else`.toList.corresponds(readerSchema.`else`.toList)(identicalSubschema) &&
      identicalSubschemaMap(writerSchema.dependentSchemas, readerSchema.dependentSchemas) &&
      writerSchema.items.toList.corresponds(readerSchema.items.toList)(identicalSubschema) &&
      writerSchema.prefixItems.toList.corresponds(readerSchema.prefixItems.toList)((w, r) =>
        w.corresponds(r)(identicalSubschema)
      ) &&
      writerSchema.contains.toList.corresponds(readerSchema.contains.toList)(identicalSubschema) &&
      writerSchema.unevaluatedItems.toList.corresponds(readerSchema.unevaluatedItems.toList)(identicalSubschema) &&
      identicalSubschemaMap(writerSchema.properties, readerSchema.properties) &&
      identicalSubschemaMap(writerSchema.patternProperties, readerSchema.patternProperties) &&
      writerSchema.additionalProperties.toList.corresponds(readerSchema.additionalProperties.toList)(
        identicalSubschema
      ) &&
      writerSchema.propertyNames.toList.corresponds(readerSchema.propertyNames.toList)(identicalSubschema) &&
      writerSchema.unevaluatedProperties.toList.corresponds(readerSchema.unevaluatedProperties.toList)(
        identicalSubschema
      )
    }

  private def removeSubschemas(schema: Schema): Schema =
    schema.copy(
      $defs = None,
      allOf = Nil,
      anyOf = Nil,
      oneOf = Nil,
      not = None,
      `if` = None,
      `then` = None,
      `else` = None,
      dependentSchemas = ListMap.empty,
      items = None,
      prefixItems = None,
      contains = None,
      unevaluatedItems = None,
      properties = ListMap.empty,
      patternProperties = ListMap.empty,
      additionalProperties = None,
      propertyNames = None,
      unevaluatedProperties = None
    )

  /** Checks if schema is for a _primitive_ value, i.e. a string, boolean, number or null */
  private def isPrimitiveSchema(s: Schema): Boolean =
    s.`type`.exists { types =>
      types.nonEmpty && !types.contains(SchemaType.Array) && !types.contains(SchemaType.Object)
    } && s == Schema( // check if the schema contains only primitive type assertions
      `type` = s.`type`,
      `enum` = s.`enum`,
      const = s.`const`,
      format = s.format,
      multipleOf = s.multipleOf,
      minimum = s.minimum,
      exclusiveMinimum = s.exclusiveMinimum,
      maximum = s.maximum,
      exclusiveMaximum = s.exclusiveMaximum,
      maxLength = s.maxLength,
      minLength = s.minLength,
      pattern = s.pattern
    )

  private def isCollectionSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Array) &&
      s.items.isDefined &&
      s == Schema(
        `type` = s.`type`,
        items = s.items,
        maxItems = s.maxItems,
        minItems = s.minItems,
        uniqueItems = s.uniqueItems
      )

  private def isTupleSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Array) &&
      s.prefixItems.isDefined &&
      s == Schema(
        `type` = s.`type`,
        prefixItems = s.prefixItems,
        maxItems = s.maxItems,
        minItems = s.minItems
      )

  private def isMapSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Object) &&
      s.additionalProperties.isDefined &&
      s == Schema(
        `type` = s.`type`,
        propertyNames = s.propertyNames,
        additionalProperties = s.additionalProperties,
        maxProperties = s.maxProperties,
        minProperties = s.minProperties
      )

  private def isProductSchema(s: Schema): Boolean =
    s.`type`.getOrElse(Nil).filter(_ != SchemaType.Null) == List(SchemaType.Object) &&
      s.properties.nonEmpty &&
      s == Schema(
        `type` = s.`type`,
        properties = s.properties,
        required = s.required,
        dependentRequired = s.dependentRequired
      )

  private def isUnionSchema(s: Schema): Boolean =
    // exactly one of `oneOf` and `anyOf` should be non-empty
    (s.oneOf.nonEmpty != s.anyOf.nonEmpty) && s == Schema(
      oneOf = s.oneOf,
      anyOf = s.anyOf,
      discriminator = s.discriminator
    )

  private def isDiscriminatedUnionSchema(s: Schema): Boolean =
    s.discriminator.nonEmpty && isUnionSchema(s)

  private def getTypes(schema: Schema): Option[List[SchemaType]] = schema match {
    case Schema.Empty   => Some(SchemaType.Values)
    case Schema.Nothing => Some(Nil)
    case s              => s.`type`
  }

  /** Checks if all writer types are compatible with at least one reader type. This check assumes schemas that have at
    * least one `type` defined.
    */
  private def checkType(writerSchema: Schema, readerSchema: Schema): Option[TypeMismatch] =
    for {
      writerTypes <- getTypes(writerSchema)
      readerTypes <- getTypes(readerSchema)
      incompatibleWriterTypes =
        writerTypes.filter(wtpe => !readerTypes.exists(rtpe => typesCompatible(wtpe, rtpe)))
      if incompatibleWriterTypes.nonEmpty
    } yield TypeMismatch(incompatibleWriterTypes, readerTypes)

  private def typesCompatible(writerTpe: SchemaType, readerTpe: SchemaType): Boolean =
    (writerTpe, readerTpe) match {
      case (SchemaType.Integer, SchemaType.Number) => true
      case _                                       => writerTpe == readerTpe
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
      case (SchemaFormat.Int32, SchemaFormat.Int64)                       => true
      case (SchemaFormat.Float | SchemaFormat.Int32, SchemaFormat.Double) => true
      case _                                                              => writerFormat == readerFormat
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

  private def checkNumericBounds(writerSchema: Schema, readerSchema: Schema): Option[NumericBoundsMismatch] = {
    val writerBounds = bounds(writerSchema)
    val readerBounds = bounds(readerSchema)
    if (readerBounds.contains(writerBounds)) None
    else Some(NumericBoundsMismatch(writerBounds, readerBounds))
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

  private def checkUniqueItems(writerSchema: Schema, readerSchema: Schema): Option[UniqueItemsRequired.type] =
    (writerSchema.uniqueItems, readerSchema.uniqueItems) match {
      case (None | Some(false), Some(true)) => Some(UniqueItemsRequired)
      case _                                => None
    }

  private def arrayLengthBounds(schema: Schema): Bounds[Int] = Bounds(
    Some(Bound.inclusive(schema.minItems.getOrElse(0))),
    schema.maxItems.map(Bound.inclusive)
  )

  private def checkArrayLengthBounds(writerSchema: Schema, readerSchema: Schema): Option[ArrayLengthBoundsMismatch] = {
    val writerBounds = arrayLengthBounds(writerSchema)
    val readerBounds = arrayLengthBounds(readerSchema)
    if (readerBounds.contains(writerBounds)) None
    else Some(ArrayLengthBoundsMismatch(writerBounds, readerBounds))
  }

  private def checkItems(writerSchema: Schema, readerSchema: Schema): Option[IncompatibleItems] = {
    val writerItems = writerSchema.items.getOrElse(Schema.Empty)
    val readerItems = readerSchema.items.getOrElse(Schema.Empty)
    compare(writerItems, readerItems) match {
      case Nil    => None
      case issues => Some(IncompatibleItems(issues))
    }
  }

  private def objectMinMaxProperties(schema: Schema): Bounds[Int] = Bounds(
    Some(Bound.inclusive(schema.minProperties.getOrElse(0))),
    schema.maxProperties.map(Bound.inclusive)
  )

  private def checkMinMaxProperties(writerSchema: Schema, readerSchema: Schema): Option[ObjectSizeBoundsMismatch] = {
    val writerBounds = objectMinMaxProperties(writerSchema)
    val readerBounds = objectMinMaxProperties(readerSchema)
    if (readerBounds.contains(writerBounds)) None
    else Some(ObjectSizeBoundsMismatch(writerBounds, readerBounds))
  }

  private def checkRequiredProperties(writerSchema: Schema, readerSchema: Schema): Option[MissingRequiredProperties] = {
    val newRequired = readerSchema.required.toSet -- writerSchema.required.toSet
    if (newRequired.isEmpty) None
    else Some(MissingRequiredProperties(newRequired))
  }

  private def checkDependentRequired(
      writerSchema: Schema,
      readerSchema: Schema
  ): List[MissingDependentRequiredProperties] =
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
        else Some(MissingDependentRequiredProperties(property, moreRequired))
      }
    }

  private def checkDiscriminatorProp(
      writerSchema: Schema,
      readerSchema: Schema
  ): Option[DiscriminatorPropertyMismatch] = for {
    writerDiscProp <- writerSchema.discriminator.map(_.propertyName)
    readerDiscProp <- readerSchema.discriminator.map(_.propertyName)
    if writerDiscProp != readerDiscProp
  } yield DiscriminatorPropertyMismatch(writerDiscProp, readerDiscProp)

  private def checkDiscriminatorValues(
      writerMapping: ListMap[String, SchemaLike],
      readerMapping: ListMap[String, SchemaLike]
  ): Option[UnsupportedDiscriminatorValues] = {
    val unsupportedValues = writerMapping.keySet -- readerMapping.keySet
    if (unsupportedValues.nonEmpty)
      Some(UnsupportedDiscriminatorValues(unsupportedValues.toList))
    else None
  }

  private def checkPropertyNames(
      writerSchema: Schema,
      readerSchema: Schema
  ): Option[IncompatiblePropertyNames] = {
    val writerPropertyNames = writerSchema.propertyNames.getOrElse(Schema(SchemaType.String))
    val readerPropertyNames = readerSchema.propertyNames.getOrElse(Schema(SchemaType.String))
    compare(writerPropertyNames, readerPropertyNames) match {
      case Nil    => None
      case issues => Some(IncompatiblePropertyNames(issues))
    }
  }

  private def checkAdditionalProperties(
      writerSchema: Schema,
      readerSchema: Schema
  ): Option[IncompatibleAdditionalProperties] = {
    val writerProps = writerSchema.additionalProperties.getOrElse(Schema.Empty)
    val readerProps = readerSchema.additionalProperties.getOrElse(Schema.Empty)
    compare(writerProps, readerProps) match {
      case Nil    => None
      case issues => Some(IncompatibleAdditionalProperties(issues))
    }
  }

}
