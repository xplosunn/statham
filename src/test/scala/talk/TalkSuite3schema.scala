package talk

import com.github.xplosunn.statham.{JsonDescriptor, JsonSchema}
import com.github.xplosunn.statham.JsonSchema.*

class TalkSuite3schema extends munit.FunSuite {

  test("have a schema for your json") {
    case class Person(name: String)

    val descriptor: JsonDescriptor[Person] = JsonDescriptor.auto.autoDerived

    val schema = descriptor.schema

    assertEquals(
      schema,
      JsonSchema.JsonObject(
        Map(
          "name" -> JsonSchema.JsonString,
        )
      )
    )
  }

  // What can I do with a schema?
  // - compare

  test("compare with self") {
    case class Person(name: String)

    val descriptor: JsonDescriptor[Person] = JsonDescriptor.auto.autoDerived

    val schema = descriptor.schema

    assertEquals(
      JsonSchema.canRead(schema, schema),
      Nil
    )
  }

  // Team workflow
  // - on every release -> store the schema in a file and `git commit`
  // - on CI -> check that changes doesn't break previous schemas
  // - when older versions are no longer needed -> delete them from git

  test("compare with next version") {
    case class Person(name: String)

    val descriptor: JsonDescriptor[Person] = JsonDescriptor.auto.autoDerived
    val schema = descriptor.schema

    case class PersonNew(name: String, email: String)
    val newSchema =  JsonDescriptor.auto.autoDerived[PersonNew].schema

    assertEquals(
      JsonSchema.canRead(schema, newSchema),
      Nil
    )
  }

  // Aspirations
  // - create client code
}
