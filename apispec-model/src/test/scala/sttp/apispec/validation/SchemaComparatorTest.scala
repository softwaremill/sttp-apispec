package sttp.apispec.validation

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.{ExampleSingleValue, Pattern, Schema, SchemaFormat, SchemaType}

import scala.collection.immutable.ListMap

class SchemaComparatorTest extends AnyFunSuite {
  private val stringSchema = Schema(SchemaType.String)
  private val integerSchema = Schema(SchemaType.Integer)
  private val numberSchema = Schema(SchemaType.Number)
  private val booleanSchema = Schema(SchemaType.Boolean)
  private val arraySchema = Schema(SchemaType.Array)
  private val objectSchema = Schema(SchemaType.Object)

  // A schema with internal structure currently not understood by SchemaComparator.
  // Such Schema can only be compared for equality (this may change in the future as SchemaComparator is improved).
  private val opaqueSchema = Schema(
    allOf = List(
      stringSchema.copy(pattern = Some(Pattern("[a-z]+"))),
      stringSchema.copy(pattern = Some(Pattern("aaa"))),
    )
  )

  private val writerTreeSchema = Schema(SchemaType.Object).copy(
    properties = ListMap(
      "value" -> stringSchema,
      "left" -> ref("WriterTree"),
      "right" -> ref("WriterTree")
    )
  )

  private val readerTreeSchema = Schema(SchemaType.Object).copy(
    properties = ListMap(
      "value" -> stringSchema,
      "left" -> ref("ReaderTree"),
      "right" -> ref("ReaderTree")
    )
  )

  private val strictReaderTreeSchema = Schema(SchemaType.Object).copy(
    properties = ListMap(
      "value" -> stringSchema,
      "left" -> ref("StrictReaderTree"),
      "right" -> ref("StrictReaderTree")
    ),
    required = List("value")
  )

  private val sharedSchemas = Map(
    "String" -> stringSchema,
    "Integer" -> integerSchema,
    "Number" -> numberSchema,
  )

  private val writerSchemas = sharedSchemas ++ Map(
    "Something" -> stringSchema,
    "WriterTree" -> writerTreeSchema
  )

  private val readerSchemas = sharedSchemas ++ Map(
    "Something" -> integerSchema,
    "ReaderTree" -> readerTreeSchema,
    "StrictReaderTree" -> strictReaderTreeSchema
  )

  private def ref(name: String): Schema =
    Schema.referenceTo(SchemaComparator.RefPrefix, name)

  private def compare(writerSchema: Schema, readerSchema: Schema): List[SchemaCompatibilityIssue] =
    SchemaComparator.compare(writerSchema, readerSchema, writerSchemas, readerSchemas)

  test("ignoring annotations") {
    assert(compare(
      stringSchema.copy(title = Some("SomeTitle")),
      stringSchema.copy(title = Some("OtherTitle"))
    ) == Nil)
  }

  test("comparing with empty schema") {
    assert(compare(Schema.Nothing, Schema.Empty) == Nil)
    assert(compare(Schema.Empty, Schema.Empty) == Nil)
    assert(compare(stringSchema, Schema.Empty) == Nil)
    assert(compare(opaqueSchema, Schema.Empty) == Nil)
    assert(compare(Schema.Empty, stringSchema) == List(
      TypeMismatch(SchemaType.Values.filter(_ != SchemaType.String), List(SchemaType.String))
    ))
  }

  test("comparing with Nothing schema") {
    assert(compare(Schema.Nothing, Schema.Nothing) == Nil)
    assert(compare(Schema.Nothing, stringSchema) == Nil)
    assert(compare(Schema.Nothing, opaqueSchema) == Nil)
    assert(compare(Schema.Empty, Schema.Nothing) == List(NoValuesAllowed(Schema.Empty)))
    assert(compare(stringSchema, Schema.Nothing) == List(NoValuesAllowed(stringSchema)))
    assert(compare(opaqueSchema, Schema.Nothing) == List(NoValuesAllowed(opaqueSchema)))
  }

  test("type checking") {
    assert(compare(integerSchema, numberSchema) == Nil)
    assert(compare(stringSchema, integerSchema) == List(
      TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
    ))
    assert(compare(Schema(SchemaType.String, SchemaType.Integer), integerSchema) == List(
      TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
    ))
    assert(compare(
      Schema(SchemaType.String, SchemaType.Integer),
      Schema(SchemaType.Boolean, SchemaType.Integer),
    ) == List(
      TypeMismatch(List(SchemaType.String), List(SchemaType.Boolean, SchemaType.Integer))
    ))
    assert(compare(
      Schema(SchemaType.String, SchemaType.Integer),
      Schema(SchemaType.Boolean, SchemaType.Integer, SchemaType.String),
    ) == Nil)
  }

  test("reference resolution") {
    assert(compare(ref("String"), ref("String")) == Nil)
    assert(compare(ref("String"), ref("Integer")) == List(
      TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
    ))
    assert(compare(ref("Something"), ref("Something")) == List(
      TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
    ))
    assert(compare(ref("Integer"), ref("Something")) == Nil)
    assert(compare(ref("Something"), ref("String")) == Nil)
  }

  test("comparing recursive schemas") {
    assert(compare(writerTreeSchema, readerTreeSchema) == Nil)
    assert(compare(writerTreeSchema, strictReaderTreeSchema) == List(
      MissingRequiredProperties(Set("value"))
    ))
  }

  test("comparing nullable schemas") {
    assert(compare(stringSchema, stringSchema.nullable) == Nil)
    assert(compare(opaqueSchema, opaqueSchema.nullable) == Nil)
    assert(compare(ref("String"), ref("String").nullable) == Nil)
    assert(compare(integerSchema.nullable, numberSchema.nullable) == Nil)

    assert(compare(stringSchema.nullable, stringSchema) == List(
      TypeMismatch(List(SchemaType.Null), List(SchemaType.String))
    ))
    assert(compare(opaqueSchema.nullable, opaqueSchema) == List(
      GeneralSchemaMismatch(opaqueSchema.nullable, opaqueSchema) //TODO better issue?
    ))
    assert(compare(ref("String").nullable, ref("String")) == List(
      GeneralSchemaMismatch(ref("String").nullable, stringSchema) //TODO better issue?
    ))
  }

  test("enum and const checking") {
    def enums(values: Any*): List[ExampleSingleValue] =
      values.toList.map(ExampleSingleValue)

    def enumSchema(values: String*): Schema = values.toList match {
      case single :: Nil => stringSchema.copy(`enum` = Some(List(single).map(ExampleSingleValue)))
      case multiple => stringSchema.copy(`enum` = Some(multiple.map(ExampleSingleValue)))
    }

    assert(compare(enumSchema("a"), stringSchema) == Nil)
    assert(compare(enumSchema("a"), enumSchema("a")) == Nil)
    assert(compare(enumSchema("a"), enumSchema("a", "b")) == Nil)
    assert(compare(enumSchema("a", "b"), enumSchema("a", "b", "c")) == Nil)

    assert(compare(stringSchema, enumSchema("a", "b")) == List(
      EnumMismatch(None, enums("a", "b"))
    ))
    assert(compare(enumSchema("a"), enumSchema("b")) == List(
      EnumMismatch(Some(enums("a")), enums("b"))
    ))
    assert(compare(enumSchema("a"), enumSchema("b", "c")) == List(
      EnumMismatch(Some(enums("a")), enums("b", "c"))
    ))
    assert(compare(enumSchema("a", "b"), enumSchema("c")) == List(
      EnumMismatch(Some(enums("a", "b")), enums("c"))
    ))
    assert(compare(enumSchema("a", "b"), enumSchema("b", "c")) == List(
      EnumMismatch(Some(enums("a")), enums("b", "c"))
    ))
  }

  test("format checking") {
    assert(compare(
      stringSchema,
      stringSchema.copy(format = Some(SchemaFormat.Date)),
    ) == List(
      FormatMismatch(None, SchemaFormat.Date)
    ))

    assert(compare(
      stringSchema.copy(format = Some(SchemaFormat.Binary)),
      stringSchema.copy(format = Some(SchemaFormat.Date)),
    ) == List(
      FormatMismatch(Some(SchemaFormat.Binary), SchemaFormat.Date)
    ))

    assert(compare(
      integerSchema.copy(format = Some(SchemaFormat.Int64)),
      integerSchema.copy(format = Some(SchemaFormat.Int32)),
    ) == List(
      FormatMismatch(Some(SchemaFormat.Int64), SchemaFormat.Int32)
    ))

    assert(compare(
      stringSchema.copy(format = Some(SchemaFormat.Date)),
      stringSchema,
    ) == Nil)

    assert(compare(
      integerSchema.copy(format = Some(SchemaFormat.Int32)),
      integerSchema.copy(format = Some(SchemaFormat.Int64)),
    ) == Nil)

    assert(compare(
      numberSchema.copy(format = Some(SchemaFormat.Float)),
      numberSchema.copy(format = Some(SchemaFormat.Double)),
    ) == Nil)
  }

  test("numerical assertions checking") {
    assert(compare(
      integerSchema.copy(
        multipleOf = Some(BigDecimal(2)),
        minimum = Some(BigDecimal(1)),
        maximum = Some(BigDecimal(10)),
      ),
      integerSchema,
    ) == Nil)

    assert(compare(
      integerSchema.copy(
        multipleOf = Some(BigDecimal(4)),
        minimum = Some(BigDecimal(2)),
        maximum = Some(BigDecimal(9)),
      ),
      integerSchema.copy(
        multipleOf = Some(BigDecimal(2)),
        minimum = Some(BigDecimal(1)),
        maximum = Some(BigDecimal(10)),
      ),
    ) == Nil)

    assert(compare(
      integerSchema,
      integerSchema.copy(
        multipleOf = Some(BigDecimal(2)),
        minimum = Some(BigDecimal(1)),
        maximum = Some(BigDecimal(10)),
      )
    ) == List(
      MultipleOfMismatch(None, BigDecimal(2)),
      NumericBoundsMismatch(Bounds(None, None), Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10))))
    ))

    assert(compare(
      integerSchema.copy(
        multipleOf = Some(BigDecimal(3)),
        minimum = Some(BigDecimal(1)),
        maximum = Some(BigDecimal(10)),
      ),
      integerSchema.copy(
        multipleOf = Some(BigDecimal(2)),
        exclusiveMinimum = Some(BigDecimal(1)),
        maximum = Some(BigDecimal(10)),
      )
    ) == List(
      MultipleOfMismatch(Some(BigDecimal(3)), BigDecimal(2)),
      NumericBoundsMismatch(
        Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10))),
        Bounds(Some(Bound.exclusive(1)), Some(Bound.inclusive(10)))
      )
    ))
  }

  test("string assertions checking") {
    assert(compare(
      stringSchema.copy(
        maxLength = Some(10),
        minLength = Some(1),
        pattern = Some(Pattern("[a-z]+")),
      ),
      stringSchema,
    ) == Nil)

    assert(compare(
      stringSchema.copy(
        maxLength = Some(8),
        minLength = Some(3),
        pattern = Some(Pattern("[a-z]+")),
      ),
      stringSchema.copy(
        maxLength = Some(10),
        minLength = Some(1),
      ),
    ) == Nil)

    assert(compare(
      stringSchema,
      stringSchema.copy(
        maxLength = Some(10),
        minLength = Some(1),
        pattern = Some(Pattern("[a-z]+")),
      ),
    ) == List(
      StringLengthBoundsMismatch(
        Bounds(Some(Bound.inclusive(0)), None),
        Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10)))),
      PatternMismatch(None, Pattern("[a-z]+"))
    ))

    assert(compare(
      stringSchema.copy(
        maxLength = Some(10),
        minLength = Some(1),
        pattern = Some(Pattern("[a-z]+")),
      ),
      stringSchema.copy(
        maxLength = Some(10),
        minLength = Some(2),
        pattern = Some(Pattern("[A-Z]+")),
      ),
    ) == List(
      StringLengthBoundsMismatch(
        Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10))),
        Bounds(Some(Bound.inclusive(2)), Some(Bound.inclusive(10)))),
      PatternMismatch(Some(Pattern("[a-z]+")), Pattern("[A-Z]+"))
    ))
  }

  test("product properties checking") {
    assert(compare(
      objectSchema.copy(
        properties = ListMap(
          "a" -> stringSchema,
          "b" -> integerSchema,
          "c" -> booleanSchema,
          "d" -> stringSchema,
        ),
        required = List("a", "b", "d")
      ),
      objectSchema.copy(
        properties = ListMap(
          "a" -> stringSchema,
          "b" -> numberSchema,
          "c" -> stringSchema,
          "e" -> stringSchema,
          "f" -> stringSchema,
        ),
        required = List("a", "b", "e"),
        dependentRequired = ListMap("c" -> List("f"))
      ),
    ) == List(
      MissingRequiredProperties(Set("e")),
      MissingDependentRequiredProperties("c", Set("f")),
      IncompatibleProperty("c", List(
        TypeMismatch(List(SchemaType.Boolean), List(SchemaType.String))
      )),
    ))
  }

  test("comparing collection schemas") {
    assert(compare(
      arraySchema.copy(
        items = Some(integerSchema),
        minItems = Some(1),
        maxItems = Some(5),
        uniqueItems = Some(true),
      ),
      arraySchema.copy(
        items = Some(numberSchema)
      ),
    ) == Nil)

    assert(compare(
      arraySchema.copy(
        items = Some(integerSchema),
        minItems = Some(1),
        maxItems = Some(5),
        uniqueItems = Some(true),
      ),
      arraySchema.copy(
        items = Some(numberSchema),
        minItems = Some(1),
        maxItems = Some(10),
        uniqueItems = Some(true),
      ),
    ) == Nil)

    assert(compare(
      arraySchema.copy(
        items = Some(integerSchema),
        minItems = Some(1),
        maxItems = Some(5),
      ),
      arraySchema.copy(
        items = Some(stringSchema),
        minItems = Some(2),
        maxItems = Some(4),
        uniqueItems = Some(true),
      ),
    ) == List(
      ArrayLengthBoundsMismatch(
        Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(5))),
        Bounds(Some(Bound.inclusive(2)), Some(Bound.inclusive(4)))
      ),
      UniqueItemsRequired,
      IncompatibleItems(List(
        TypeMismatch(List(SchemaType.Integer), List(SchemaType.String))
      ))
    ))
  }

  test("comparing tuple schemas") {
    assert(compare(
      arraySchema.copy(
        prefixItems = Some(List(integerSchema, stringSchema, booleanSchema)),
      ),
      arraySchema.copy(
        prefixItems = Some(List(numberSchema, stringSchema)),
      ),
    ) == Nil)

    assert(compare(
      arraySchema.copy(
        prefixItems = Some(List(integerSchema, stringSchema)),
      ),
      arraySchema.copy(
        prefixItems = Some(List(numberSchema, booleanSchema, stringSchema)),
      ),
    ) == List(
      IncompatiblePrefixItem(1, List(
        TypeMismatch(List(SchemaType.String), List(SchemaType.Boolean))
      )),
      IncompatiblePrefixItem(2, List(
        TypeMismatch(SchemaType.Values.filter(_ != SchemaType.String), List(SchemaType.String))
      ))
    ))
  }

  test("comparing map schemas") {
    assert(compare(
      objectSchema.copy(
        additionalProperties = Some(integerSchema),
        propertyNames = Some(stringSchema.copy(maxLength = Some(8))),
        minProperties = Some(1),
        maxProperties = Some(10),
      ),
      objectSchema.copy(
        additionalProperties = Some(numberSchema),
      ),
    ) == Nil)

    assert(compare(
      objectSchema.copy(
        additionalProperties = Some(integerSchema),
        propertyNames = Some(stringSchema.copy(maxLength = Some(8))),
        minProperties = Some(2),
        maxProperties = Some(10),
      ),
      objectSchema.copy(
        additionalProperties = Some(numberSchema),
        propertyNames = Some(stringSchema.copy(maxLength = Some(10))),
        minProperties = Some(1),
        maxProperties = Some(12),
      ),
    ) == Nil)

    assert(compare(
      objectSchema.copy(
        additionalProperties = Some(stringSchema),
      ),
      objectSchema.copy(
        additionalProperties = Some(numberSchema),
        propertyNames = Some(stringSchema.copy(maxLength = Some(10))),
        minProperties = Some(1),
        maxProperties = Some(12),
      ),
    ) == List(
      IncompatibleAdditionalProperties(List(
        TypeMismatch(List(SchemaType.String), List(SchemaType.Number))
      )),
      IncompatiblePropertyNames(List(
        StringLengthBoundsMismatch(
          Bounds(Some(Bound.inclusive(0)), None),
          Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(10)))
        )
      )),
      ObjectSizeBoundsMismatch(
        Bounds(Some(Bound.inclusive(0)), None),
        Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(12))
      )
    )))
  }
}
