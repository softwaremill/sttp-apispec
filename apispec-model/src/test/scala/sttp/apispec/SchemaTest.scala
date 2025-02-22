package sttp.apispec

import org.scalatest.funsuite.AnyFunSuite

class SchemaTest extends AnyFunSuite {
  test("nullable with type") {
    val schema = Schema(SchemaType.String)
    assert(schema.nullable == Schema(SchemaType.String, SchemaType.Null))
    assert(schema.nullable.nullable == schema.nullable) // idempotency
  }

  test("nullable without type") {
    val schema = Schema()
    assert(schema.nullable == Schema(anyOf = List(schema, Schema.Null)))
    assert(schema.nullable.nullable == schema.nullable) // idempotency
  }

  test("nullable with anyOf") {
    val schema = Schema(anyOf = List(Schema(SchemaType.String), Schema(SchemaType.Number)))
    assert(schema.nullable == Schema(anyOf = List(Schema(SchemaType.String), Schema(SchemaType.Number), Schema.Null)))
    assert(schema.nullable.nullable == schema.nullable) // idempotency
  }

  test("nullable enum") {
    val schema =
      Schema(`type` = Some(List(SchemaType.String)), `enum` = Some(List("a", "b").map(ExampleSingleValue(_))))
    assert(
      schema.nullable == Schema(
        `type` = Some(List(SchemaType.String, SchemaType.Null)),
        `enum` = Some(List("a", "b", "null").map(ExampleSingleValue(_)))
      )
    )
    assert(schema.nullable.nullable == schema.nullable) // idempotency
  }

  test("nullable const") {
    val schema = Schema(`type` = Some(List(SchemaType.String)), `const` = Some(ExampleSingleValue("a")))
    assert(
      schema.nullable == Schema(
        `type` = Some(List(SchemaType.String, SchemaType.Null)),
        `enum` = Some(List("a", "null").map(ExampleSingleValue(_)))
      )
    )
    assert(schema.nullable.nullable == schema.nullable)
  }
}
