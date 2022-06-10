package com.k3rnl.collect.language

import com.k3rnl.collect.evaluator.Evaluator
import com.k3rnl.collect.evaluator.Evaluator.FunctionNative
import com.k3rnl.collect.language.AST.{MapType, RuntimeValue, StringType}
import com.k3rnl.collect.evaluator.MapEngine._

object BuiltInFunctions {
  val functions: Map[String, Evaluator.FunctionDeclaration] = Map(
    "typeof" -> new FunctionNative("typeof", List("expression"), context => {
      println(context.env("expression").typeOf)
      null
    }),
    "print" -> new FunctionNative("print", List("message"), context => {
      println(context.env("message").value.toString)
      null
    }),
    "firstMatchingValue" -> new FunctionNative("firstMatchingValue", List("from", "regexp"), context => {
      val regex = context.env("regexp").value.toString.r
      regex.findFirstMatchIn(context.env("from").value.toString) match {
        case Some(m) => RuntimeValue(m.group(1), StringType)
        case None => null
      }
    }),
    "find" -> new FunctionNative("find", List("regexp"), context => {
      val regex = ("^" + context.env("regexp").value.toString).r
      RuntimeValue(context.env.find("", regex), MapType)
    }),
    "match" -> new FunctionNative("match", List("regexp"), context => {
      val regex = ("^" + context.env("regexp").value.toString).r
      RuntimeValue(context.env.findFirst("", regex).get, MapType)
    }),
    "value" -> new FunctionNative("value", List("kv"), context => {
      RuntimeValue(context.env("kv").value.asInstanceOf[(String, String)]._2, StringType)
    })
  )
}
