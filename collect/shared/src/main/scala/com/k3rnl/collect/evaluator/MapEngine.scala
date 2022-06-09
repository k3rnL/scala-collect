package com.k3rnl.collect.evaluator

import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.RuntimeValue

import scala.util.matching.Regex

object MapEngine {
  implicit class RegexVariableSearcher(val regex: Regex) {
    lazy val keyRegex: Regex = regex.regex.split("=").head.r
    lazy val valueRegex: Regex = regex.regex.split("=").last.r
  }

  implicit class MapEngine(val map: Map[String, RuntimeValue]) {
    def find(prefix: String, regex: Regex): Iterable[(String, String)] = {
      val prefixWithPoint = prefix match {
        case "" => ""
        case _ => prefix + "."
      }
      map.flatMap({
        case (key, value) => value.typeOf match {
          case AST.MapType(_, _) =>
            value.value.asInstanceOf[Map[String, RuntimeValue]].find(prefixWithPoint + key, regex)
          case _ => regex.findFirstMatchIn(prefixWithPoint + key + "=" + value.value) match {
              case Some(m) => (prefixWithPoint + key, m.subgroups.mkString) :: Nil
              case None => None
            }
        }
      })
    }

//    def find(regex: Regex): Option[Any] =

  }
}

