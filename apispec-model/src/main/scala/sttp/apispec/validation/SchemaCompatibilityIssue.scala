package sttp.apispec.validation

import sttp.apispec.{ExampleSingleValue, ExampleValue, Pattern, Schema, SchemaType}

sealed abstract class SchemaCompatibilityIssue extends Product {
  def description: String

  override def toString: String = s"$productPrefix($description)"

  protected def pluralize(what: String, coll: Seq[Any]): String =
    if (coll.lengthCompare(1) == 0) s"$what ${coll.head}"
    else s"${what}s ${coll.mkString(", ")}"

  protected def form(coll: Seq[Any], singular: String, plural: String): String =
    if (coll.size == 1) singular else plural
}

/**
 * A [[SchemaCompatibilityIssue]] used when the schemas are different but the comparator was unable
 * to determine compatibility. Thus, the schemas may or may not be compatible.
 */
case class GeneralSchemaMismatch(
  writerSchema: Schema,
  readerSchema: Schema
) extends SchemaCompatibilityIssue {
  def description: String =
    "schemas differ, but it was not possible to determine their compatibility"
}

case class NoValuesAllowed(
  writerSchema: Schema
) extends SchemaCompatibilityIssue {
  def description: String = "target schema does not allow any values"
}

case class TypeMismatch(
  incompatibleWriterTypes: List[SchemaType],
  readerTypes: List[SchemaType]
) extends SchemaCompatibilityIssue {
  def description: String = {
    val wt = incompatibleWriterTypes
    val writerTypesRepr =
      s"${pluralize("type", wt)} ${form(wt, "is", "are")} incompatible with ${form(readerTypes, "it", "them")}"
    s"target schema is restricted to ${pluralize("type", readerTypes)}, $writerTypesRepr"
  }
}

case class EnumMismatch(
  // None indicates that the writer schema has no enum values
  incompatibleWriterValues: Option[List[ExampleValue]],
  readerValues: List[ExampleValue]
) extends SchemaCompatibilityIssue {
  def description: String = {
    val writerValuesRepr =
      incompatibleWriterValues.fold("")(values =>
        s", ${pluralize("value", values)} ${form(values, "is", "are")} incompatible with ${form(readerValues, "it", "them")}"
      )
    s"target schema is restricted to ${pluralize("value", readerValues)}$writerValuesRepr"
  }
}

case class FormatMismatch(
  writerFormat: Option[String],
  readerFormat: String
) extends SchemaCompatibilityIssue {
  def description: String = {
    val writerFormatRepr = writerFormat.fold("")(wf => s" with which $wf is not compatible")
    s"target schema is restricted to format $readerFormat$writerFormatRepr"
  }
}

case class MultipleOfMismatch(
  writerMultiplier: Option[BigDecimal],
  readerMultiplier: BigDecimal
) extends SchemaCompatibilityIssue {
  def description: String = {
    val writerMultiplierRepr = writerMultiplier.fold("")(wm => s", as opposed to $wm")
    s"target schema accepts multiples of $readerMultiplier$writerMultiplierRepr"
  }
}

case class BoundsMismatch(
  writerBounds: Bounds[BigDecimal],
  readerBounds: Bounds[BigDecimal]
) extends SchemaCompatibilityIssue {
  def description: String =
    s"target value bounds $readerBounds are stricter than $writerBounds"
}

case class StringLengthBoundsMismatch(
  writerBounds: Bounds[Int],
  readerBounds: Bounds[Int]
) extends SchemaCompatibilityIssue {
  def description: String =
    s"target string length bounds $readerBounds are stricter than $writerBounds"
}

case class PatternMismatch(
  writerPattern: Option[Pattern],
  readerPattern: Pattern
) extends SchemaCompatibilityIssue {
  def description: String = {
    val writerPatternRepr = writerPattern.fold("")(wp => s", as opposed to $wp")
    s"target schema is restricted to pattern $readerPattern$writerPatternRepr"
  }
}

case object UniqueItemsIntroduced extends SchemaCompatibilityIssue {
  def description: String = "target schema requires unique items"
}

case class ArrayLengthBoundsMismatch(
  writerBounds: Bounds[Int],
  readerBounds: Bounds[Int]
) extends SchemaCompatibilityIssue {
  def description: String =
    s"target array length (minItems/maxItems) bounds $readerBounds are stricter than $writerBounds"
}

case class MinMaxPropertiesMismatch(
  writerBounds: Bounds[Int],
  readerBounds: Bounds[Int]
) extends SchemaCompatibilityIssue {
  def description: String =
    s"target object size (minProperties/maxProperties) $readerBounds is stricter than $writerBounds"
}

case class MoreRequiredProperties(
  newRequiredProperties: Set[String]
) extends SchemaCompatibilityIssue {
  def description: String = s"target schema introduced new required properties: ${newRequiredProperties.mkString(", ")}"
}

case class MoreDependentRequired(
  property: String,
  newRequiredProperties: Set[String]
) extends SchemaCompatibilityIssue {
  def description: String =
    s"target schema introduced new required dependent properties for property $property: ${newRequiredProperties.mkString(", ")}"
}

case class DiscriminatorPropertyMismatch(
  writerDiscriminator: Option[String],
  readerDiscriminator: String
) extends SchemaCompatibilityIssue {
  def description: String = {
    val writerDiscriminatorRepr = writerDiscriminator.fold("")(wd => s", as opposed to $wd")
    s"target schema requires discriminator property $readerDiscriminator$writerDiscriminatorRepr"
  }
}

case class DiscriminatorValuesMismatch(
  unsupportedValues: List[String]
) extends SchemaCompatibilityIssue {
  def description: String =
    s"target schema does not accept discriminator values: ${unsupportedValues.mkString(", ")}"
}

/** Base class for compatibility issues which aggregate issues from a subschema. A _subschema_ is a schema used within a
 * structure of another schema, e.g. a schema for an array item, a property, etc.
 */
sealed abstract class SubschemaCompatibilityIssue extends SchemaCompatibilityIssue {
  def subschemaIssues: List[SchemaCompatibilityIssue]

  protected def issuesRepr: String =
    subschemaIssues.iterator
      .map(i => s"- ${i.description.replace("\n", "\n  ")}")
      .mkString("\n")
}

case class IncompatibleProperty(
  property: String,
  subschemaIssues: List[SchemaCompatibilityIssue]
) extends SubschemaCompatibilityIssue {
  def description: String =
    s"incompatible schema for property $property:\n$issuesRepr"
}

case class IncompatibleDiscriminatorCase(
  discriminatorValue: String,
  subschemaIssues: List[SchemaCompatibilityIssue]
) extends SubschemaCompatibilityIssue {
  def description: String =
    s"incompatible schema for discriminator value $discriminatorValue:\n$issuesRepr"
}

case class IncompatibleAdditionalProperties(
  subschemaIssues: List[SchemaCompatibilityIssue]
) extends SubschemaCompatibilityIssue {
  def description: String =
    s"incompatible schema for additional properties:\n$issuesRepr"
}

case class IncompatiblePropertyNames(
  subschemaIssues: List[SchemaCompatibilityIssue]
) extends SubschemaCompatibilityIssue {
  override def description: String =
    s"incompatible schema for property names:\n$issuesRepr"
}

case class IncompatibleItems(
  subschemaIssues: List[SchemaCompatibilityIssue]
) extends SubschemaCompatibilityIssue {
  def description: String =
    s"incompatible schema for items:\n$issuesRepr"
}

case class IncompatiblePrefixItem(
  index: Int,
  subschemaIssues: List[SchemaCompatibilityIssue]
) extends SubschemaCompatibilityIssue {
  def description: String =
    s"incompatible schema for prefix item at index $index:\n$issuesRepr"
}
