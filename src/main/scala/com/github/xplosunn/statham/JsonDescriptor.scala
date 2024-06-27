package com.github.xplosunn.statham

sealed trait JsonDescriptor[T] {
  def schema: JsonSchema
  final def write(value: T): String = JsonDescriptor.circeWrite(value, this)
  final def read(json: String): Either[JsonError, T] = JsonDescriptor.circeRead(json, this)
  final def humanDoc: String = JsonDescriptor.humanDoc(this)

  final def dimap[New](from: T => New, into: New => T): JsonDescriptor[New] =
    JsonDescriptor.JsonDiMap(this, from, into)
}

object JsonDescriptor {
  case class JsonDiMap[T, Underlying](underlying: JsonDescriptor[Underlying], from: Underlying => T, into: T => Underlying) extends JsonDescriptor[T] {
    override def schema: JsonSchema = underlying.schema
  }
  implicit case object JsonString extends JsonDescriptor[String] {
    override def schema: JsonSchema = JsonSchema.JsonString
  }
  implicit case object JsonBoolean extends JsonDescriptor[Boolean] {
    override def schema: JsonSchema = JsonSchema.JsonBoolean
  }
  case class JsonOption[T](of: JsonDescriptor[T]) extends JsonDescriptor[Option[T]] {
    override def schema: JsonSchema = JsonSchema.JsonArray(of.schema)
  }
  case class JsonList[T](of: JsonDescriptor[T]) extends JsonDescriptor[List[T]] {
    override def schema: JsonSchema = JsonSchema.JsonArray(of.schema)
  }
  case class JsonObject[T](unsafeConstructor: Map[String, Any] => T, fields: Map[String, JsonObject.Field[T, ?]]) extends JsonDescriptor[T] {
    override def schema: JsonSchema = JsonSchema.JsonObject(fields.view.mapValues(_.descriptor.schema).toMap)
  }
  object JsonObject {
    case class Field[Object, Field](access: Object => Field, comment: Option[String], descriptor: JsonDescriptor[Field])
  }
  case class JsonStringHardcoded[T](value: String, scalaValue: T) extends JsonDescriptor[T] {
    override def schema: JsonSchema = JsonSchema.JsonStringHardcoded(value)
  }
  case class JsonOneOf[T](possibilities: List[JsonOneOf.One[T, ?]]) extends JsonDescriptor[T] {
    override def schema: JsonSchema = JsonSchema.JsonOneOf(possibilities.map(_.descriptor.schema))
  }
  object JsonOneOf {
    case class One[T, Subtype](attemptDowncast: T => Option[Subtype], supercast: Subtype => T, descriptor: JsonDescriptor[Subtype])
  }

  implicit def jsonList[T](implicit of: JsonDescriptor[T]): JsonList[T] = JsonList(of)
  implicit def jsonOption[T](implicit of: JsonDescriptor[T]): JsonOption[T] = JsonOption(of)

  private def circeReadJson(json: String): Either[JsonError, io.circe.Json] = {
   io.circe.parser.parse(json).left.map(parsingFailure => JsonError())
  }

  private def circeReadMatch[T](json: io.circe.Json, descriptor: JsonDescriptor[T]): Either[JsonError, T] = {
    descriptor match {
      case JsonDiMap(underlying, from, into) => circeReadMatch(json, underlying).map(from)
      case JsonOption(of) => descriptor match {
        case JsonOption(of) =>
          json.fold[Either[JsonError, T]](
            jsonNull = Right(None),
            jsonBoolean = _ => circeReadMatch(json, of).map(Some(_)),
            jsonNumber = _ => circeReadMatch(json, of).map(Some(_)),
            jsonString = _ => circeReadMatch(json, of).map(Some(_)),
            jsonArray = _ => circeReadMatch(json, of).map(Some(_)),
            jsonObject = _ => circeReadMatch(json, of).map(Some(_)),
          )
        case descriptor => Left(JsonError())
      }
      case JsonOneOf(possibilities) =>
        scala.util.boundary {
          for (elem <- possibilities) {
            circeReadMatch(json, elem.descriptor) match {
              case Right(value) => scala.util.boundary.break(Right(elem.supercast(value)))
              case Left(value) =>
            }
          }
          Left(JsonError())
        }
      case descriptor =>
        json.fold[Either[JsonError, T]](
          jsonNull = Left(JsonError()),
          jsonBoolean = boolean => descriptor match {
            case JsonBoolean => Right(boolean)
            case _ => Left(JsonError())
          },
          jsonNumber = number => ???,
          jsonString = string => descriptor match {
            case JsonString => Right(string)
            case JsonStringHardcoded(value, scalaValue) => Either.cond(string == value, scalaValue, JsonError())
            case _ => Left(JsonError())
          },
          jsonArray = {
            import cats.syntax.traverse.*
            import cats.instances.vector.catsStdInstancesForVector
            array => descriptor match {
              case JsonList(of) => array.traverse(circeReadMatch(_, of)).map(_.toList)
              case _ => Left(JsonError())
            }
          },
          jsonObject = jsonObject => descriptor match {
            case JsonObject(unsafeConstructor, fields) =>
              val jsonMap = jsonObject.toMap
              import cats.syntax.traverse.*
              import cats.instances.list.catsStdInstancesForList
              fields.toList.traverse {
                case (fieldKey, jsonObjectField) =>
                  jsonMap.get(fieldKey).toRight(JsonError())
                    .flatMap(circeReadMatch(_, jsonObjectField.descriptor))
                    .map(fieldKey -> _)
              }.map(_.toMap)
                .map(unsafeConstructor)
            case _ => Left(JsonError())
          }
        )
    }
  }

  private def circeRead[T](json: String, descriptor: JsonDescriptor[T]): Either[JsonError, T] =
    circeReadJson(json).flatMap(circeReadMatch(_, descriptor))

  private def circeWrite[T](value: T, descriptor: JsonDescriptor[T]): String = {
    import io.circe.syntax._
    import io.circe.Json
    val printer = io.circe.Printer.noSpaces
    descriptor match {
      case JsonDiMap(underlying, from, into) => circeWrite(into(value), underlying)
      case JsonString => printer.print((value: String).asJson)
      case JsonBoolean => printer.print((value: Boolean).asJson)
      case JsonOption(of) => value match {
        case Some(value) => circeWrite(value, of)
        case None => printer.print(Json.Null)
      }
      case JsonList(of) => "[" + value.map(circeWrite(_, of)).mkString(",") + "]"
      case JsonObject(_, fields) =>
        fields.map {
          case (label, JsonObject.Field(access, _, descriptor)) =>
            s""""${label}":${circeWrite(access(value), descriptor)}"""
        }.mkString("{", ",", "}")
      case JsonStringHardcoded(value, scalaValue) => printer.print((value: String).asJson)
      case JsonOneOf(possibilities) =>
        scala.util.boundary {
          for (elem <- possibilities) {
            elem.attemptDowncast(value) match {
              case Some(value) => scala.util.boundary.break(circeWrite(value, elem.descriptor))
              case None =>
            }
          }
          //TODO
          throw RuntimeException("oops")
        }
    }
  }

  object auto extends magnolia1.AutoDerivation[JsonDescriptor] {

    import magnolia1.*

    override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = {
      def safeCast[A, B](descriptor: JsonDescriptor[A & B]): JsonDescriptor[A] =
        descriptor.asInstanceOf
      JsonOneOf(ctx.subtypes.map { subtype => JsonOneOf.One(subtype.lift, identity, safeCast(subtype.typeclass)) }.toList)
    }

    override def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
      JsonObject(valuesMap => ctx.rawConstruct(ctx.params.map(param => valuesMap.get(param.label).get).toSeq), ctx.params.map { param => param.label -> JsonObject.Field(param.deref, None, param.typeclass)}.toMap)
  }

  private def humanDoc(descriptor: JsonDescriptor[?]): String = {
    descriptor match {
      case JsonDiMap(underlying, _, _) => humanDoc(underlying)
      case JsonString => "string"
      case JsonBoolean => "boolean"
      case JsonOption(of) => humanDoc(of) + "?"
      case JsonList(of) => humanDoc(of) + "[]"
      case JsonObject(unsafeConstructor, fields) =>
        if (fields.isEmpty) "{}"
        else {
          fields.toList.sortBy(_._1).map {
            case (fieldName, field) =>
              s"""  ${fieldName}: ${humanDoc(field.descriptor)}""" + field.comment.map(" // " + _).getOrElse("") + "\n"
          }.mkString("{\n", "", "}")
        }
      case JsonStringHardcoded(value, scalaValue) => JsonString.write(value)
      case JsonOneOf(possibilities) => possibilities.map(one => humanDoc(one.descriptor)).mkString(" | ")
    }
  }
}