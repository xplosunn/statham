package com.github.xplosunn.statham

import scala.util.control.NoStackTrace

object JsonConfigure {

  case class FailedToConfigure(reason: String) extends Exception with NoStackTrace

  def configureOrThrow[T](jsonDescriptor: JsonDescriptor[T], configure: Configure => Configure): JsonDescriptor[T] =
    ???

  trait Configure { self =>
    def isObject(configure: ObjectConfigure => ObjectConfigure): self.type
    def isStringOneOf(possibleValues: List[String]): self.type
  }

  trait ObjectFieldConfigure extends Configure {
    def addComment(comment: String): ObjectFieldConfigure
  }

  trait ObjectConfigure {
    def hasField(field: String, configure: ObjectFieldConfigure => ObjectFieldConfigure): ObjectConfigure
  }

}
