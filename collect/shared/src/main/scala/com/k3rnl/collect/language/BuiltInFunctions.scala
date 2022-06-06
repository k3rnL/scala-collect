package com.k3rnl.collect.language

import com.k3rnl.collect.Evaluator
import com.k3rnl.collect.Evaluator.FunctionNative
import com.k3rnl.collect.language.AST.{RuntimeValue, StringType}

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
        case Some(m) => new RuntimeValue(m.group(1), StringType)
        case None => null
      }
    })
  )
}
