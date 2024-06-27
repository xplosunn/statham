package com.github.xplosunn.statham

import com.github.xplosunn.statham.JsonSchema.*

class JsonSchemaSuite extends munit.FunSuite {
  test("canRead") {
    val newSchema = JsonBoolean
    val oldSchema = JsonBoolean
    assertEquals(canRead(newSchema, oldSchema), Nil)
    assertEquals(canRead(newSchema, JsonOneOf(List(oldSchema))), Nil)
    assertEquals(canRead(JsonOneOf(List(newSchema)), oldSchema), Nil)
    assertEquals(canRead(JsonOneOf(List(newSchema, JsonString)), oldSchema), Nil)
    assertEquals(canRead(JsonOneOf(List(newSchema, JsonStringHardcoded("some_value"))), oldSchema), Nil)
  }

  test("canRead object with one less field") {
    val newSchema = JsonObject(Map(
      "field1" -> JsonBoolean
    ))
    val oldSchema = JsonObject(Map(
      "field1" -> JsonBoolean,
      "field2" -> JsonBoolean
    ))
    assertEquals(canRead(newSchema, oldSchema), Nil)
  }

  test("canRead object with missing field as optional") {
    val newSchema = JsonObject(Map(
      "field1" -> JsonBoolean,
      "field2" -> JsonOption(JsonBoolean)
    ))
    val oldSchema = JsonObject(Map(
      "field1" -> JsonBoolean
    ))
    assertEquals(canRead(newSchema, oldSchema), Nil)
  }

  test("canRead enum with one more possibility") {
    val newSchema = JsonOneOf(List(JsonStringHardcoded("red"), JsonStringHardcoded("blue"), JsonStringHardcoded("green")))
    val oldSchema = JsonOneOf(List(JsonStringHardcoded("red"), JsonStringHardcoded("green")))
    assertEquals(canRead(newSchema, oldSchema), Nil)
  }

  test("canRead string from enum") {
    val newSchema = JsonString
    val oldSchema = JsonOneOf(List(JsonStringHardcoded("red"), JsonStringHardcoded("green")))
    assertEquals(canRead(newSchema, oldSchema), Nil)
  }

  test("canRead option from non-option") {
    val newSchema = JsonOption(JsonString)
    val oldSchema = JsonOneOf(List(JsonStringHardcoded("red"), JsonStringHardcoded("green")))
    assertEquals(canRead(newSchema, oldSchema), Nil)
  }
}
