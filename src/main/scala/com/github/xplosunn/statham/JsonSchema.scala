package com.github.xplosunn.statham

sealed trait JsonSchema

object JsonSchema {
  case object JsonString extends JsonSchema
  case object JsonBoolean extends JsonSchema
  case class JsonOption(of: JsonSchema) extends JsonSchema
  case class JsonArray(of: JsonSchema) extends JsonSchema
  case class JsonObject(fields: Map[String, JsonSchema]) extends JsonSchema

  case class JsonStringHardcoded(value: String) extends JsonSchema
  case class JsonOneOf(possibilities: List[JsonSchema]) extends JsonSchema

  case class CompatibilityIssue(message: String)

  def canRead(newSchema: JsonSchema, oldSchema: JsonSchema): List[CompatibilityIssue] = {
    def incompatible(): List[CompatibilityIssue] = {
      List(CompatibilityIssue(s"can't read old $oldSchema with $newSchema"))
    }
    def anyMatch(old: JsonSchema, newPossibilities: List[JsonSchema]): List[CompatibilityIssue] = {
      scala.util.boundary {
        for (newPossibility <- newPossibilities) {
          if (canRead(newPossibility, old).isEmpty) {
            scala.util.boundary.break(Nil)
          }
        }
        incompatible()
      }
    }

    clearOneOfsWithSingleElement(oldSchema) match {
      case old @ JsonString => clearOneOfsWithSingleElement(newSchema) match {
        case JsonOneOf(newPossibilities) => anyMatch(old, newPossibilities)
        case JsonOption(newOf) => canRead(newOf, old)
        case JsonString => Nil
        case _ => incompatible()
      }
      case old @ JsonBoolean => clearOneOfsWithSingleElement(newSchema) match {
        case JsonOneOf(newPossibilities) => anyMatch(old, newPossibilities)
        case JsonOption(newOf) => canRead(newOf, old)
        case JsonBoolean => Nil
        case _ => incompatible()
      }
      case old @ JsonOption(ofOld) => clearOneOfsWithSingleElement(newSchema) match {
        case JsonOneOf(newPossibilities) => anyMatch(old, newPossibilities)
        case JsonOption(ofNew) => canRead(ofNew, ofOld)
        case _ => incompatible()
      }
      case old @ JsonArray(ofOld) => clearOneOfsWithSingleElement(newSchema) match {
        case JsonOneOf(newPossibilities) => anyMatch(old, newPossibilities)
        case JsonOption(newOf) => canRead(newOf, old)
        case JsonArray(ofNew) => canRead(ofNew, ofOld)
        case _ => incompatible()
      }
      case old @ JsonObject(oldFields) => clearOneOfsWithSingleElement(newSchema) match {
        case JsonOneOf(newPossibilities) => anyMatch(old, newPossibilities)
        case JsonOption(newOf) => canRead(newOf, old)
        case JsonObject(newFields) =>
          newFields.toList.flatMap {
            case (fieldName, newSchema) =>
              oldFields.get(fieldName).map(canRead(newSchema, _)).getOrElse(if (canBeNull(newSchema)) Nil else incompatible())
            }
        case _ => incompatible()
      }
      case old @ JsonStringHardcoded(oldValue) => clearOneOfsWithSingleElement(newSchema) match {
        case JsonOneOf(newPossibilities) => anyMatch(old, newPossibilities)
        case JsonOption(newOf) => canRead(newOf, old)
        case JsonString => Nil
        case JsonStringHardcoded(newValue) => if (oldValue == newValue) Nil else incompatible()
        case _ => incompatible()
      }
      case old @ JsonOneOf(oldPossibilities) =>
        clearOneOfsWithSingleElement(newSchema) match {
          case JsonOneOf(newPossibilities) =>
            oldPossibilities.foldLeft(List.empty[CompatibilityIssue])((issues, oldPossibility) => issues ++ anyMatch(oldPossibility, newPossibilities))
          case JsonOption(newOf) => canRead(newOf, old)
          case newSchema =>
            oldPossibilities.foldLeft(List.empty[CompatibilityIssue])((issues, oldPossibility) => issues ++ canRead(newSchema, oldPossibility))
        }
    }
  }

  private def canBeNull(jsonSchema: JsonSchema): Boolean = {
    jsonSchema match {
      case JsonOption(_) => true
      case JsonOneOf(possibilities) => possibilities.exists(canBeNull)
      case JsonString => false
      case JsonBoolean => false
      case JsonArray(_) => false
      case JsonObject(_) => false
      case JsonStringHardcoded(_) => false
    }
  }

  private def clearOneOfsWithSingleElement(jsonSchema: JsonSchema): JsonSchema = {
    jsonSchema match {
      case JsonString => JsonString
      case JsonBoolean => JsonBoolean
      case JsonOption(of) => JsonOption(clearOneOfsWithSingleElement(of))
      case JsonArray(of) => JsonArray(clearOneOfsWithSingleElement(of))
      case JsonObject(fields) => JsonObject(fields.view.mapValues(clearOneOfsWithSingleElement).toMap)
      case s: JsonStringHardcoded => s
      case JsonOneOf(possibilities) =>
        possibilities.map(clearOneOfsWithSingleElement) match {
          case singleElem :: Nil => singleElem
          case list => JsonOneOf(list)
        }
    }
  }

}
