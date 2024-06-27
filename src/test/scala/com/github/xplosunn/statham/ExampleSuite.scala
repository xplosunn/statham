package com.github.xplosunn.statham

import com.github.xplosunn.statham.JsonSchema.*

class ExampleSuite extends munit.FunSuite {
  // Let's take the following types:
  case class PersonId(id: String)

  sealed trait Person
  case class Teacher(id: PersonId, partTime: Boolean) extends Person
  case class Student(id: PersonId) extends Person

  test("write json") {
    implicit val personIdJsonDescriptor: JsonDescriptor[PersonId] = implicitly[JsonDescriptor[String]].dimap(PersonId.apply, _.id)
    val studentJsonDescriptor: JsonDescriptor[Student] = JsonDescriptor.auto.autoDerived

    val expected = """{"id":"studentId"}"""
    val obtained = studentJsonDescriptor.write(Student(PersonId("studentId")))

    assertEquals(obtained, expected)
  }

  test("read json") {
    implicit val personIdJsonDescriptor: JsonDescriptor[PersonId] = implicitly[JsonDescriptor[String]].dimap(PersonId.apply, _.id)
    val studentJsonDescriptor: JsonDescriptor[Student] = JsonDescriptor.auto.autoDerived

    val expected = Student(PersonId("studentId"))
    val obtained: Either[JsonError, Person] = studentJsonDescriptor.read("""{"id":"studentId"}""")

    assertEquals(obtained, Right(expected))
  }

  test("compare schemas") {
    implicit val personIdJsonDescriptor: JsonDescriptor[PersonId] = implicitly[JsonDescriptor[String]].dimap(PersonId.apply, _.id)
    val studentJsonDescriptor: JsonDescriptor[Student] = JsonDescriptor.auto.autoDerived
    val teacherJsonDescriptor: JsonDescriptor[Teacher] = JsonDescriptor.auto.autoDerived

    // If your API used to take a Teacher as input and now takes a Student, that's okay (one less field)
    val noIssues = JsonSchema.canRead(newSchema = studentJsonDescriptor.schema, oldSchema = teacherJsonDescriptor.schema)
    assertEquals(noIssues, Nil)

    // If your API used to take a Student as input and now takes a Teacher, that's not okay (one more field)
    val oneIssue = JsonSchema.canRead(newSchema = teacherJsonDescriptor.schema, oldSchema = studentJsonDescriptor.schema)
    val expected = CompatibilityIssue("can't read old JsonObject(Map(id -> JsonString)) with JsonObject(Map(id -> JsonString, partTime -> JsonBoolean))")
    assertEquals(oneIssue, List(expected))
  }

  test("get documentation") {
    implicit val personIdJsonDescriptor: JsonDescriptor[PersonId] = implicitly[JsonDescriptor[String]].dimap(PersonId.apply, _.id)
    val teacherJsonDescriptor: JsonDescriptor[Teacher] = JsonDescriptor.auto.autoDerived

    val obtained = teacherJsonDescriptor.humanDoc
    val expected =
      """{
        |  id: string
        |  partTime: boolean
        |}""".stripMargin
    assertEquals(obtained, expected)
  }

  test("amend schemas") {
    implicit val personIdJsonDescriptor: JsonDescriptor[PersonId] = implicitly[JsonDescriptor[String]].dimap(PersonId.apply, _.id)
    val teacherJsonDescriptor: JsonDescriptor[Teacher] = JsonDescriptor.auto.autoDerived

    // Not implemented yet :)

//    val updatedTeacherJsonDescriptor: JsonDescriptor[Teacher] =
//      JsonConfigure.configureOrThrow(
//        teacherJsonDescriptor,
//        _.isObject(
//          _.hasField(
//            "partTime",
//            _.addComment("true when the teacher only works part time")
//          )
//        )
//      )

//    val obtained = updatedTeacherJsonDescriptor.humanDoc
//    val expected =
//      """{
//        |  id: string
//        |  partTime: boolean // true when the teacher only works part time
//        |}""".stripMargin
//    assertEquals(obtained, expected)
  }
}
