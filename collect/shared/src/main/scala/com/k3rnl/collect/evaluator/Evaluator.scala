package com.k3rnl.collect.evaluator

import com.k3rnl.collect.evaluator.Evaluator.Context
import com.k3rnl.collect.evaluator.MapEngine._
import com.k3rnl.collect.language.{AST, BuiltInFunctions}
import com.k3rnl.collect.language.AST.{Expression, RuntimeValue, Statement}

import scala.annotation.tailrec

class Evaluator {

  def call(name: String, args: List[AST.Expression], context: Context): RuntimeValue = {
    val function = declaredFunctions(name)
    val subScope: Context = function.args.zip(args.map(arg => arg.run(context))).foldLeft(context)((context, arg) => context.copy(env = context.env + arg))
    stack = subScope +: stack

    val result = function.run(subScope)
    stack = stack.tail
    result
  }

  final def interpret(asts: List[AST]): Context = interpret(asts, Context(this, Map()))

  @tailrec
  final def interpret(asts: List[AST], context: Context): Evaluator.Context = asts match {
    case Nil => context
    case ast +: tail => ast match {
      case AST.Program(statements) => interpret(statements, context)
      case statement: Statement => statement match {
        case expression: Expression => expression.run(context) ; interpret(tail, context)
        case AST.Assignment(variable, value) => {
          val result = value.run(context)
          interpret(tail, context.copy(env = new MapEngine(context.env).assign(variable.path, variable.name, result)))
        }
      }
    }
  }

  var declaredFunctions: Map[String, Evaluator.FunctionDeclaration] = BuiltInFunctions.functions
  var stack: List[Context] = List()
}

object Evaluator {
  case class Context(evaluator: Evaluator, env: Map[String, RuntimeValue])

  abstract class FunctionDeclaration(val name: String, val args: List[String]) {
    def run(context: Context): RuntimeValue
  }
  class FunctionNative(name: String, args: List[String], function: Context => RuntimeValue) extends FunctionDeclaration(name, args) {
    def run(context: Context): RuntimeValue = function(context)
  }

//  abstract class FunctionPrototypenUser(name: String, args: List[String]) extends FunctionPrototype(name, args) {

}