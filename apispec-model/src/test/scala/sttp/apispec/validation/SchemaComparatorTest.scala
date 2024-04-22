package sttp.apispec.validation

import org.scalatest.funsuite.AnyFunSuite
import sttp.apispec.{Pattern, Schema, SchemaType}

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
  }

  test("recursive schemas") {
    assert(compare(writerTreeSchema, readerTreeSchema) == Nil)
    assert(compare(writerTreeSchema, strictReaderTreeSchema) == List(
      MoreRequiredProperties(Set("value"))
    ))
  }

  //TODO significantly more tests
}