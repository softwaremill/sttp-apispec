package sttp.apispec.validation

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.{ExampleSingleValue, Pattern, Schema, SchemaFormat, SchemaType}

import scala.collection.immutable.ListMap

class SchemaComparatorTest extends AnyFunSuite {
  private val stringSchema = Schema(SchemaType.String)
  private val integerSchema = Schema(SchemaType.Integer)
  private val numberSchema = Schema(SchemaType.Number)

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
      GeneralSchemaMismatch(Schema.Empty, stringSchema) //TODO: better issue for this case
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

  test("type mismatch") {
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

  test("recursive schemas") {
    assert(compare(writerTreeSchema, readerTreeSchema) == Nil)
    assert(compare(writerTreeSchema, strictReaderTreeSchema) == List(
      MoreRequiredProperties(Set("value"))
    ))
  }

  test("nullable schemas") {
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

  test("enum and const mismatch") {
    def enums(values: Any*): List[ExampleSingleValue] =
      values.toList.map(ExampleSingleValue)

    def enumSchema(values: String*): Schema = values.toList match {
      case single :: Nil => stringSchema.copy(enum = Some(List(single).map(ExampleSingleValue)))
      case multiple => stringSchema.copy(enum = Some(multiple.map(ExampleSingleValue)))
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

  test("format mismatch") {
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

  test("numerical assertions") {
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

  //TODO significantly more tests
}
