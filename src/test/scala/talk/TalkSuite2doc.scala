package talk

import com.github.xplosunn.statham.{JsonConfigure, JsonDescriptor}
import com.github.xplosunn.statham.JsonSchema.*

class TalkSuite2doc extends munit.FunSuite {

  test("doc") {
    case class Person(name: String)

    val descriptor: JsonDescriptor[Person] = JsonDescriptor.auto.autoDerived

    assertEquals(
      descriptor.humanDoc,
      """{
        |  name: string
        |}""".stripMargin
    )
  }

  // What if I want to document the name?

  test("doc with comment") {
    case class Person(name: String)

    try {
      val descriptor: JsonDescriptor[Person] =
        JsonConfigure.configureOrThrow(
          JsonDescriptor.auto.autoDerived[Person],
          _.isObject(
            _.hasField("name", _.addComment("it's important to know the name"))
          )
        )

      assertEquals(
        descriptor.humanDoc,
        """{
          |  name: string // it's important to know the name
          |}""".stripMargin
      )
    } catch {
      case e: NotImplementedError =>
    }
  }

  test("doc with comment without throwing?") {
    case class Person(name: String)

    val descriptor: JsonDescriptor[Person] = JsonDescriptor.auto.autoDerived

    // like circe (order matters)
    // val descriptor: JsonDescriptor[Person] = JsonDescriptor.forProduct1("name")(Person.apply)(_.name)
    
    // like spray (order & field names matter)
    // val descriptor: JsonDescriptor[Person] = JsonDescriptor.jsonFormat1(Person.apply)
  }
}
