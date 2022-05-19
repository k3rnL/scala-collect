package com.k3rnl.collect.language

import com.k3rnl.collect.Evaluator
import com.k3rnl.collect.Evaluator.FunctionNative

object BuiltInFunctions {
  val functions: Map[String, Evaluator.FunctionDeclaration] = Map(
    "print" -> new FunctionNative("print", List("message"), context => {
      println(context.env("message").toString)
      null
    }),
    "firstMatchingValue" -> new FunctionNative("firstMatchingValue", List("from", "regexp"), context => {
      val regex = context.env("regexp").toString.r
      regex.findFirstMatchIn(context.env("from").toString) match {
        case Some(m) => m.group(1)
        case None => null
      }
    })
  )
}
