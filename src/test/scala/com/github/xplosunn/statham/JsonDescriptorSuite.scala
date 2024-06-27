package com.github.xplosunn.statham

import com.github.xplosunn.statham.JsonDescriptor.*

class JsonDescriptorSuite extends munit.FunSuite {

  // test instances
  implicitly[JsonDescriptor[String]]
  implicitly[JsonDescriptor[Boolean]]
  implicitly[JsonDescriptor[Option[String]]]
  implicitly[JsonDescriptor[List[String]]]

  test("dimap") {
    case class Wrapper(string: String)

    assertEquals(
      implicitly[JsonDescriptor[String]].dimap(Wrapper.apply, _.string).schema,
      JsonString.schema
    )
  }

  case class DescriptorAndValue[T](descriptor: JsonDescriptor[T], value: T)

  List[(String, DescriptorAndValue[?], String)](
    (
      "dimapped",
      {
        case class Wrapper(string: String)
        DescriptorAndValue(
          implicitly[JsonDescriptor[String]].dimap(Wrapper.apply, _.string),
          Wrapper("wrapped")
        )
      },
      "\"wrapped\""
    ),
    (
      "null",
      {
        DescriptorAndValue(
          implicitly[JsonDescriptor[Option[String]]],
          None
        )
      },
      "null"
    ),
    (
      "nullable string",
      {
        DescriptorAndValue(
          implicitly[JsonDescriptor[Option[String]]],
          Some("str")
        )
      },
      "\"str\""
    ),
    (
      "nullable list null",
      {
        DescriptorAndValue(
          implicitly[JsonDescriptor[Option[List[String]]]],
          None
        )
      },
      "null"
    ),
    (
      "nullable list empty",
      {
        DescriptorAndValue(
          implicitly[JsonDescriptor[Option[List[String]]]],
          Some(Nil)
        )
      },
      "[]"
    ),
    (
      "nullable list nonempty",
      {
        DescriptorAndValue(
          implicitly[JsonDescriptor[Option[List[String]]]],
          Some(List(""))
        )
      },
      "[\"\"]"
    ),
    (
      "auto derived class",
      {
        case class Wrapper(string: String)
        val wrapperDescriptor = {
          import JsonDescriptor.auto.autoDerived
          implicitly[JsonDescriptor[Wrapper]]
        }
        DescriptorAndValue(
          wrapperDescriptor,
          Wrapper("str")
        )
      },
      "{\"string\":\"str\"}"
    ),
    (
      "auto derived object",
      {
        case object Wrapper
        val wrapperDescriptor = {
          import JsonDescriptor.auto.autoDerived
          implicitly[JsonDescriptor[Wrapper.type]]
        }
        DescriptorAndValue(
          wrapperDescriptor,
          Wrapper
        )
      },
      "{}"
    ),
  ).foreach { case (testLabel, DescriptorAndValue(descriptor, value), json) =>
    test(s"write ${testLabel}") {
      assertEquals(descriptor.write(value), json)
    }
    test(s"read ${testLabel}") {
      assertEquals(descriptor.read(json), Right(value))
    }
  }
}
