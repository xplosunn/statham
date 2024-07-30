package talk

import com.github.xplosunn.statham.JsonDescriptor
import com.github.xplosunn.statham.JsonSchema.*

class TalkSuite3adt extends munit.FunSuite {

  test("adt") {
    sealed trait Person
    case class Student(name: String) extends Person
    case class Teacher(name: String) extends Person

    val descriptor: JsonDescriptor[Person] = JsonDescriptor.auto.autoDerived

    assertEquals(
      descriptor.humanDoc,
      """{
        |  name: string
        |} | {
        |  name: string
        |}""".stripMargin
    )
  }
  
  // Do we force the user to provide a discriminant field?
  // if we do -> less flexible
  // if we don't -> pitfall
}
