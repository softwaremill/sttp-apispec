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
}
