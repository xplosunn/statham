package talk

import com.github.xplosunn.statham.JsonDescriptor
import com.github.xplosunn.statham.JsonSchema.*

class TalkSuite1rw extends munit.FunSuite {

  // Contents:
  // - Main concepts of Statham
  // - Trade-offs in case class <-> json
  // - JSON backwards compatibility
  // - Automated compatibility checking

  // - configure or throw

  // - Client code generate

  test("read / write json (circe)") {
    case class Person(name: String)

    val codec: io.circe.Codec[Person] = io.circe.Codec.forProduct1("name")(Person.apply)(_.name)

    assertEquals(
      codec(Person("me")).noSpaces,
      "{\"name\":\"me\"}"
    )
    assertEquals(
      io.circe.parser.parse("{\"name\":\"me\"}").flatMap(codec.decodeJson),
      Right(Person("me")),
    )
  }

  test("read / write json") {
    case class Person(name: String)

    val descriptor: JsonDescriptor[Person] = JsonDescriptor.auto.autoDerived

    assertEquals(
      descriptor.write(Person("me")),
      "{\"name\":\"me\"}"
    )
    assertEquals(
      descriptor.read("{\"name\":\"me\"}"),
      Right(Person("me"))
    )
  }
}
