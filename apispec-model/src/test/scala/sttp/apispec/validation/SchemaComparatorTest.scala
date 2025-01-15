package sttp.apispec.validation

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec._

import scala.collection.immutable.ListMap

abstract class SchemaComparatorTest(referencePrefix: String) extends AnyFunSuite {

  private val stringSchema = Schema(SchemaType.String)
  private val integerSchema = Schema(SchemaType.Integer)
  private val numberSchema = Schema(SchemaType.Number)
  private val booleanSchema = Schema(SchemaType.Boolean)
  private val arraySchema = Schema(SchemaType.Array)
  private val objectSchema = Schema(SchemaType.Object)

  // case schemas for coproduct
  private val writerFooSchema = objectSchema.copy(
    properties = ListMap(
      "type" -> stringSchema,
      "value" -> stringSchema
    ),
    required = List("type", "value")
  )

  private val readerFooSchema = objectSchema.copy(
    properties = ListMap(
      "type" -> stringSchema,
      "value" -> stringSchema,
      "flag" -> booleanSchema
    ),
    required = List("type", "value")
  )

  private val barSchema = objectSchema.copy(
    properties = ListMap(
      "type" -> stringSchema,
      "value" -> integerSchema
    ),
    required = List("type", "value")
  )

  private val bazSchema = objectSchema.copy(
    properties = ListMap(
      "type" -> stringSchema,
      "value" -> booleanSchema,
      "extra" -> stringSchema
    ),
    required = List("type", "value", "extra")
  )

  // A schema with internal structure currently not understood by SchemaComparator.
  // Such Schema can only be compared for equality (this may change in the future as SchemaComparator is improved).
  private val opaqueSchema = Schema(
    allOf = List(
      stringSchema.copy(pattern = Some(Pattern("[a-z]+"))),
      stringSchema.copy(pattern = Some(Pattern("aaa")))
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
    "Number" -> numberSchema
  )

  private val writerSchemas = sharedSchemas ++ Map(
    "Something" -> stringSchema,
    "WriterTree" -> writerTreeSchema,
    // coproduct cases
    "Foo" -> writerFooSchema,
    "Bar" -> barSchema
  )

  private val readerSchemas = sharedSchemas ++ Map(
    "Something" -> integerSchema,
    "ReaderTree" -> readerTreeSchema,
    "StrictReaderTree" -> strictReaderTreeSchema,
    // coproduct cases
    "Foo" -> readerFooSchema,
    "Bar" -> barSchema,
    "Baz" -> bazSchema
  )

  private def ref(name: String): Schema =
    Schema.referenceTo(referencePrefix, name)

  private def compare(writerSchema: Schema, readerSchema: Schema): List[SchemaCompatibilityIssue] =
    new SchemaComparator(writerSchemas, readerSchemas)
      .compare(writerSchema, readerSchema)

  test("ignoring annotations") {
    assert(
      compare(
        stringSchema.copy(title = Some("SomeTitle")),
        stringSchema.copy(title = Some("OtherTitle"))
      ) == Nil
    )
  }

  test("comparing with empty schema") {
    assert(compare(Schema.Nothing, Schema.Empty) == Nil)
    assert(compare(Schema.Empty, Schema.Empty) == Nil)
    assert(compare(stringSchema, Schema.Empty) == Nil)
    assert(compare(opaqueSchema, Schema.Empty) == Nil)
    assert(
      compare(Schema.Empty, stringSchema) == List(
        TypeMismatch(SchemaType.Values.filter(_ != SchemaType.String), List(SchemaType.String))
      )
    )
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
    assert(
      compare(stringSchema, integerSchema) == List(
        TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
      )
    )
    assert(
      compare(Schema(SchemaType.String, SchemaType.Integer), integerSchema) == List(
        TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
      )
    )
    assert(
      compare(
        Schema(SchemaType.String, SchemaType.Integer),
        Schema(SchemaType.Boolean, SchemaType.Integer)
      ) == List(
        TypeMismatch(List(SchemaType.String), List(SchemaType.Boolean, SchemaType.Integer))
      )
    )
    assert(
      compare(
        Schema(SchemaType.String, SchemaType.Integer),
        Schema(SchemaType.Boolean, SchemaType.Integer, SchemaType.String)
      ) == Nil
    )
  }

  test("reference resolution") {
    assert(compare(ref("String"), ref("String")) == Nil)
    assert(
      compare(ref("String"), ref("Integer")) == List(
        TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
      )
    )
    assert(
      compare(ref("Something"), ref("Something")) == List(
        TypeMismatch(List(SchemaType.String), List(SchemaType.Integer))
      )
    )
    assert(compare(ref("Integer"), ref("Something")) == Nil)
    assert(compare(ref("Something"), ref("String")) == Nil)
  }

  test("opaque schemas with identical reference to different schemas") {
    // `allOf` is currently not understood by SchemaComparator,
    // so the schemas are opaque and are compared for pure structural equality
    val schema = Schema(allOf = List(ref("Something"), ref("String")))
    assert(
      compare(schema, schema) == List(
        GeneralSchemaMismatch(schema, schema)
      )
    )
  }

  test("opaque schemas with different references to identical schemas") {
    // `allOf` is currently not understood by SchemaComparator,
    // so the schemas are opaque and are compared for pure structural equality
    assert(
      compare(
        Schema(allOf = List(ref("Something"), ref("Integer"))),
        Schema(allOf = List(ref("String"), ref("Integer")))
      ) == Nil
    )
  }

  test("comparing recursive schemas") {
    assert(compare(writerTreeSchema, readerTreeSchema) == Nil)
    assert(
      compare(writerTreeSchema, strictReaderTreeSchema) == List(
        MissingRequiredProperties(Set("value"))
      )
    )
  }

  test("compatible nullable schemas") {
    assert(compare(stringSchema, stringSchema.nullable) == Nil)
    assert(compare(opaqueSchema, opaqueSchema.nullable) == Nil)
    assert(compare(ref("String"), ref("String").nullable) == Nil)
    assert(compare(integerSchema.nullable, numberSchema.nullable) == Nil)
  }

  test("incompatible nullable schemas") {
    assert(
      compare(stringSchema.nullable, stringSchema) == List(
        TypeMismatch(List(SchemaType.Null), List(SchemaType.String))
      )
    )
    assert(
      compare(opaqueSchema.nullable, opaqueSchema) == List(
        IncompatibleUnionVariant(1, List(GeneralSchemaMismatch(Schema.Null, opaqueSchema)))
      )
    )
    assert(
      compare(ref("String").nullable, ref("String")) == List(
        IncompatibleUnionVariant(1, List(TypeMismatch(List(SchemaType.Null), List(SchemaType.String))))
      )
    )
  }

  private def enums(values: Any*): List[ExampleSingleValue] =
    values.toList.map(ExampleSingleValue)

  private def enumSchema(values: String*): Schema = values.toList match {
    case single :: Nil => stringSchema.copy(`enum` = Some(List(single).map(ExampleSingleValue)))
    case multiple      => stringSchema.copy(`enum` = Some(multiple.map(ExampleSingleValue)))
  }

  test("compatible enum & const") {
    assert(compare(enumSchema("a"), stringSchema) == Nil)
    assert(compare(enumSchema("a"), enumSchema("a")) == Nil)
    assert(compare(enumSchema("a"), enumSchema("a", "b")) == Nil)
    assert(compare(enumSchema("a", "b"), enumSchema("a", "b", "c")) == Nil)
  }

  test("incompatible enum & const") {
    assert(
      compare(stringSchema, enumSchema("a", "b")) == List(
        EnumMismatch(None, enums("a", "b"))
      )
    )
    assert(
      compare(enumSchema("a"), enumSchema("b")) == List(
        EnumMismatch(Some(enums("a")), enums("b"))
      )
    )
    assert(
      compare(enumSchema("a"), enumSchema("b", "c")) == List(
        EnumMismatch(Some(enums("a")), enums("b", "c"))
      )
    )
    assert(
      compare(enumSchema("a", "b"), enumSchema("c")) == List(
        EnumMismatch(Some(enums("a", "b")), enums("c"))
      )
    )
    assert(
      compare(enumSchema("a", "b"), enumSchema("b", "c")) == List(
        EnumMismatch(Some(enums("a")), enums("b", "c"))
      )
    )
  }

  test("compatible formats") {
    assert(
      compare(
        stringSchema.copy(format = Some(SchemaFormat.Date)),
        stringSchema
      ) == Nil
    )

    assert(
      compare(
        integerSchema.copy(format = Some(SchemaFormat.Int32)),
        integerSchema.copy(format = Some(SchemaFormat.Int64))
      ) == Nil
    )

    assert(
      compare(
        numberSchema.copy(format = Some(SchemaFormat.Float)),
        numberSchema.copy(format = Some(SchemaFormat.Double))
      ) == Nil
    )
  }

  test("incompatible formats") {
    assert(
      compare(
        stringSchema,
        stringSchema.copy(format = Some(SchemaFormat.Date))
      ) == List(
        FormatMismatch(None, SchemaFormat.Date)
      )
    )

    assert(
      compare(
        stringSchema.copy(format = Some(SchemaFormat.Binary)),
        stringSchema.copy(format = Some(SchemaFormat.Date))
      ) == List(
        FormatMismatch(Some(SchemaFormat.Binary), SchemaFormat.Date)
      )
    )

    assert(
      compare(
        integerSchema.copy(format = Some(SchemaFormat.Int64)),
        integerSchema.copy(format = Some(SchemaFormat.Int32))
      ) == List(
        FormatMismatch(Some(SchemaFormat.Int64), SchemaFormat.Int32)
      )
    )
  }

  test("compatible numerical assertions") {
    assert(
      compare(
        integerSchema.copy(
          multipleOf = Some(BigDecimal(2)),
          minimum = Some(BigDecimal(1)),
          maximum = Some(BigDecimal(10))
        ),
        integerSchema
      ) == Nil
    )

    assert(
      compare(
        integerSchema.copy(
          multipleOf = Some(BigDecimal(4)),
          minimum = Some(BigDecimal(2)),
          maximum = Some(BigDecimal(9))
        ),
        integerSchema.copy(
          multipleOf = Some(BigDecimal(2)),
          minimum = Some(BigDecimal(1)),
          maximum = Some(BigDecimal(10))
        )
      ) == Nil
    )
  }

  test("incompatible numerical assertions") {
    assert(
      compare(
        integerSchema,
        integerSchema.copy(
          multipleOf = Some(BigDecimal(2)),
          minimum = Some(BigDecimal(1)),
          maximum = Some(BigDecimal(10))
        )
      ) == List(
        MultipleOfMismatch(None, BigDecimal(2)),
        NumericBoundsMismatch(Bounds(None, None), Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10))))
      )
    )

    assert(
      compare(
        integerSchema.copy(
          multipleOf = Some(BigDecimal(3)),
          minimum = Some(BigDecimal(1)),
          maximum = Some(BigDecimal(10))
        ),
        integerSchema.copy(
          multipleOf = Some(BigDecimal(2)),
          exclusiveMinimum = Some(BigDecimal(1)),
          maximum = Some(BigDecimal(10))
        )
      ) == List(
        MultipleOfMismatch(Some(BigDecimal(3)), BigDecimal(2)),
        NumericBoundsMismatch(
          Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10))),
          Bounds(Some(Bound.exclusive(1)), Some(Bound.inclusive(10)))
        )
      )
    )
  }

  test("compatible string assertions") {
    assert(
      compare(
        stringSchema.copy(
          maxLength = Some(10),
          minLength = Some(1),
          pattern = Some(Pattern("[a-z]+"))
        ),
        stringSchema
      ) == Nil
    )

    assert(
      compare(
        stringSchema.copy(
          maxLength = Some(8),
          minLength = Some(3),
          pattern = Some(Pattern("[a-z]+"))
        ),
        stringSchema.copy(
          maxLength = Some(10),
          minLength = Some(1)
        )
      ) == Nil
    )

  }

  test("incompatible string assertions") {
    assert(
      compare(
        stringSchema,
        stringSchema.copy(
          maxLength = Some(10),
          minLength = Some(1),
          pattern = Some(Pattern("[a-z]+"))
        )
      ) == List(
        StringLengthBoundsMismatch(
          Bounds(Some(Bound.inclusive(0)), None),
          Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10)))
        ),
        PatternMismatch(None, Pattern("[a-z]+"))
      )
    )

    assert(
      compare(
        stringSchema.copy(
          maxLength = Some(10),
          minLength = Some(1),
          pattern = Some(Pattern("[a-z]+"))
        ),
        stringSchema.copy(
          maxLength = Some(10),
          minLength = Some(2),
          pattern = Some(Pattern("[A-Z]+"))
        )
      ) == List(
        StringLengthBoundsMismatch(
          Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(10))),
          Bounds(Some(Bound.inclusive(2)), Some(Bound.inclusive(10)))
        ),
        PatternMismatch(Some(Pattern("[a-z]+")), Pattern("[A-Z]+"))
      )
    )
  }

  test("comparing product schemas") {
    assert(
      compare(
        objectSchema.copy(
          properties = ListMap(
            "a" -> stringSchema,
            "b" -> integerSchema,
            "c" -> booleanSchema,
            "d" -> stringSchema
          ),
          required = List("a", "b", "d")
        ),
        objectSchema.copy(
          properties = ListMap(
            "a" -> stringSchema,
            "b" -> numberSchema,
            "c" -> stringSchema,
            "e" -> stringSchema,
            "f" -> stringSchema
          ),
          required = List("a", "b", "e"),
          dependentRequired = ListMap("c" -> List("f"))
        )
      ) == List(
        MissingRequiredProperties(Set("e")),
        MissingDependentRequiredProperties("c", Set("f")),
        IncompatibleProperty(
          "c",
          List(
            TypeMismatch(List(SchemaType.Boolean), List(SchemaType.String))
          )
        )
      )
    )
  }

  test("compatible coproduct schemas") {
    assert(
      compare(
        Schema(
          oneOf = List(ref("Foo"), ref("Bar")),
          discriminator = Some(Discriminator("type", None))
        ),
        Schema(
          oneOf = List(ref("Foo"), ref("Bar"), ref("Baz")),
          discriminator = Some(Discriminator("type", None))
        )
      ) == Nil
    )
  }

  test("incompatible coproduct schemas") {
    assert(
      compare(
        Schema(
          oneOf = List(ref("Foo"), ref("Bar"), ref("Baz")),
          discriminator = Some(Discriminator("type", None))
        ),
        Schema(
          oneOf = List(ref("Foo"), ref("Bar")),
          discriminator = Some(Discriminator("type", None))
        )
      ) == List(
        UnsupportedDiscriminatorValues(List("Baz"))
      )
    )
  }

  test("discriminator property mismatch") {
    assert(
      compare(
        Schema(
          oneOf = List(ref("Foo"), ref("Bar")),
          discriminator = Some(Discriminator("kind", None))
        ),
        Schema(
          oneOf = List(ref("Foo"), ref("Bar"), ref("Baz")),
          discriminator = Some(Discriminator("type", None))
        )
      ) == List(
        DiscriminatorPropertyMismatch("kind", "type")
      )
    )
  }

  test("unsupported discriminator values") {
    assert(
      compare(
        Schema(
          oneOf = List(ref("Foo"), ref("Bar")),
          discriminator = Some(Discriminator("type", Some(ListMap("WFoo" -> s"${referencePrefix}Foo"))))
        ),
        Schema(
          oneOf = List(ref("Foo"), ref("Bar"), ref("Baz")),
          discriminator = Some(Discriminator("type", Some(ListMap("RBar" -> s"${referencePrefix}Bar"))))
        )
      ) == List(
        UnsupportedDiscriminatorValues(List("WFoo", "Bar"))
      )
    )
  }

  test("incompatible coproduct case schema") {
    assert(
      compare(
        Schema(
          oneOf = List(ref("Foo"), ref("Bar")),
          discriminator = Some(Discriminator("type", Some(ListMap("Baz" -> s"${referencePrefix}Bar"))))
        ),
        Schema(
          oneOf = List(ref("Foo"), ref("Baz")),
          discriminator = Some(Discriminator("type", None))
        )
      ) == List(
        IncompatibleDiscriminatorCase("Baz", compare(barSchema, bazSchema))
      )
    )
  }

  test("compatible collection schemas") {
    assert(
      compare(
        arraySchema.copy(
          items = Some(integerSchema),
          minItems = Some(1),
          maxItems = Some(5),
          uniqueItems = Some(true)
        ),
        arraySchema.copy(
          items = Some(numberSchema)
        )
      ) == Nil
    )

    assert(
      compare(
        arraySchema.copy(
          items = Some(integerSchema),
          minItems = Some(1),
          maxItems = Some(5),
          uniqueItems = Some(true)
        ),
        arraySchema.copy(
          items = Some(numberSchema),
          minItems = Some(1),
          maxItems = Some(10),
          uniqueItems = Some(true)
        )
      ) == Nil
    )
  }

  test("incompatible collection schemas") {
    assert(
      compare(
        arraySchema.copy(
          items = Some(integerSchema),
          minItems = Some(1),
          maxItems = Some(5)
        ),
        arraySchema.copy(
          items = Some(stringSchema),
          minItems = Some(2),
          maxItems = Some(4),
          uniqueItems = Some(true)
        )
      ) == List(
        ArrayLengthBoundsMismatch(
          Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(5))),
          Bounds(Some(Bound.inclusive(2)), Some(Bound.inclusive(4)))
        ),
        UniqueItemsRequired,
        IncompatibleItems(
          List(
            TypeMismatch(List(SchemaType.Integer), List(SchemaType.String))
          )
        )
      )
    )
  }

  test("compatible tuple schemas") {
    assert(
      compare(
        arraySchema.copy(
          prefixItems = Some(List(integerSchema, stringSchema, booleanSchema))
        ),
        arraySchema.copy(
          prefixItems = Some(List(numberSchema, stringSchema))
        )
      ) == Nil
    )
  }

  test("incompatible tuple schemas") {
    assert(
      compare(
        arraySchema.copy(
          prefixItems = Some(List(integerSchema, stringSchema))
        ),
        arraySchema.copy(
          prefixItems = Some(List(numberSchema, booleanSchema, stringSchema))
        )
      ) == List(
        IncompatiblePrefixItem(
          1,
          List(
            TypeMismatch(List(SchemaType.String), List(SchemaType.Boolean))
          )
        ),
        IncompatiblePrefixItem(
          2,
          List(
            TypeMismatch(SchemaType.Values.filter(_ != SchemaType.String), List(SchemaType.String))
          )
        )
      )
    )
  }

  test("compatible map schemas") {
    assert(
      compare(
        objectSchema.copy(
          additionalProperties = Some(integerSchema),
          propertyNames = Some(stringSchema.copy(maxLength = Some(8))),
          minProperties = Some(1),
          maxProperties = Some(10)
        ),
        objectSchema.copy(
          additionalProperties = Some(numberSchema)
        )
      ) == Nil
    )

    assert(
      compare(
        objectSchema.copy(
          additionalProperties = Some(integerSchema),
          propertyNames = Some(stringSchema.copy(maxLength = Some(8))),
          minProperties = Some(2),
          maxProperties = Some(10)
        ),
        objectSchema.copy(
          additionalProperties = Some(numberSchema),
          propertyNames = Some(stringSchema.copy(maxLength = Some(10))),
          minProperties = Some(1),
          maxProperties = Some(12)
        )
      ) == Nil
    )
  }

  test("incompatible map schemas") {
    assert(
      compare(
        objectSchema.copy(
          additionalProperties = Some(stringSchema)
        ),
        objectSchema.copy(
          additionalProperties = Some(numberSchema),
          propertyNames = Some(stringSchema.copy(maxLength = Some(10))),
          minProperties = Some(1),
          maxProperties = Some(12)
        )
      ) == List(
        IncompatibleAdditionalProperties(
          List(
            TypeMismatch(List(SchemaType.String), List(SchemaType.Number))
          )
        ),
        IncompatiblePropertyNames(
          List(
            StringLengthBoundsMismatch(
              Bounds(Some(Bound.inclusive(0)), None),
              Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(10)))
            )
          )
        ),
        ObjectSizeBoundsMismatch(
          Bounds(Some(Bound.inclusive(0)), None),
          Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(12)))
        )
      )
    )
  }

  test("compatible untagged union schemas") {
    assert(
      compare(
        Schema(anyOf = List(stringSchema, integerSchema)),
        Schema(anyOf = List(stringSchema, numberSchema, booleanSchema))
      ) == Nil
    )
  }

  test("incompatible untagged union schemas") {
    assert(
      compare(
        Schema(anyOf = List(stringSchema, numberSchema, booleanSchema)),
        Schema(anyOf = List(stringSchema, integerSchema))
      ) == List(
        IncompatibleUnionVariant(
          1,
          List(
            AlternativeIssues(
              List(
                List(TypeMismatch(List(SchemaType.Number), List(SchemaType.String))),
                List(TypeMismatch(List(SchemaType.Number), List(SchemaType.Integer)))
              )
            )
          )
        ),
        IncompatibleUnionVariant(
          2,
          List(
            AlternativeIssues(
              List(
                List(TypeMismatch(List(SchemaType.Boolean), List(SchemaType.String))),
                List(TypeMismatch(List(SchemaType.Boolean), List(SchemaType.Integer)))
              )
            )
          )
        )
      )
    )
  }
}
