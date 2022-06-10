package com.k3rnl.collect.evaluator

import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.{MapType, RuntimeValue, StringType}

import scala.util.matching.Regex

object MapEngine {
  implicit class RegexVariableSearcher(val regex: Regex) {
    lazy val keyRegex: Regex = regex.regex.split("=").head.r
    lazy val valueRegex: Regex = regex.regex.split("=").last.r
  }

  implicit class MapEngine(val map: Map[String, RuntimeValue]) {
    def apply(name: String): RuntimeValue = map(name)

    def assign(key: String, value: RuntimeValue): Map[String, RuntimeValue] = map.get(key) match {
      case Some(RuntimeValue(_, typeOf)) if typeOf.isInstanceOf[value.typeOf.type] => map + (key -> value)
      case Some(RuntimeValue(_, typeOf)) => throw new IllegalArgumentException(s"Cannot assign value of type ${value.typeOf} to key $key of type $typeOf")
      case None => map + (key -> value)
    }

    def assign(path: Seq[String], key: String, value: RuntimeValue, from: Map[String, RuntimeValue] = map): Map[String, RuntimeValue] = path match {
      case Nil => assign(key, value)
      case head +: tail => from.get(head) match {
        case Some(RuntimeValue(_, MapType)) => from + (head -> RuntimeValue(from.assign(tail, key, value, from(head).asInstanceOf[Map[String, RuntimeValue]]), MapType))
        case Some(RuntimeValue(_, typeOf)) => throw new IllegalArgumentException(s"Cannot assign value of type ${value.typeOf} to key $key of type $typeOf")
        case None => from + (head -> RuntimeValue(from.assign(tail, key, value, from), MapType))
      }
    }

    def get(path: Seq[String], key: String, map: Map[String, RuntimeValue] = map): Option[RuntimeValue] = path match {
      case Nil => map.get(key)
      case head :: tail => map.get(head) match {
        case Some(RuntimeValue(value, MapType)) => get(tail, key, value.asInstanceOf[Map[String, RuntimeValue]])
        case Some(_) => throw new Exception(s"$key is not a map")
        case None => None
      }
    }

    def find(prefix: String, regex: Regex): Iterable[(String, String)] = {
      val prefixWithPoint = prefix match {
        case "" => ""
        case _ => prefix + "."
      }
      map.flatMap({
        case (key, value) => value.typeOf match {
          case MapType =>
            value.value.asInstanceOf[Map[String, RuntimeValue]].find(prefixWithPoint + key, regex)
          case _ => regex.findFirstMatchIn(prefixWithPoint + key + "=" + value.value) match {
              case Some(m) => (prefixWithPoint + key, m.subgroups.mkString) :: Nil
              case None => None
            }
        }
      })
    }

    def findFirst(prefix: String, regex: Regex): Option[(String, String)] = {
      val prefixWithPoint = prefix match {
        case "" => ""
        case _ => prefix + "."
      }
      map.collectFirst({
        case (key, value) => value.typeOf match {
          case MapType =>
            value.value.asInstanceOf[Map[String, RuntimeValue]].findFirst(prefixWithPoint + key, regex) match {
              case Some(m) => m
            }
          case _ => regex.findFirstMatchIn(prefixWithPoint + key + "=" + value.value) match {
            case Some(m) => (prefixWithPoint + key, m.subgroups.mkString)
          }
        }
      })
    }

    override def toString: String = map.toString
  }
}

