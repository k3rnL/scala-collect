package com.k3rnl.collect

import com.k3rnl.collect.Evaluator.Context
import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.{Expression, Statement}

import scala.annotation.tailrec

class Evaluator {

  def call(name: String, args: List[AST.Expression], context: Context): Any = {
    val function = declaredFunctions(name)
    val subScope: Context = function.args.zip(args.map(arg => arg.run(context))).foldLeft(context)((context, arg) => context + arg)
    stack = subScope +: stack

    val result = function.run(subScope)
    stack = stack.tail
    result
  }

  @tailrec
  final def interpret(asts: List[AST], context: Context): Evaluator.Context = asts match {
    case Nil => context
    case ast +: tail => ast match {
      case AST.Program(statements) => interpret(statements, context)
      case statement: Statement => statement match {
        case AST.Assignment(name, value) => {
          interpret(tail, context + (name -> value.run(context)))
        }
      }
      case expression: Expression => expression.run(context) ; interpret(tail, context)
    }
  }

  var declaredFunctions: Map[String, Evaluator.FunctionDeclaration] = Map()
  var stack: List[Context] = List()
}

object Evaluator {
  class Context(val evaluator: Evaluator, var env: Map[String, Any]) {
    def apply(env: Map[String, Any] = env): Context = new Context(evaluator, env)

    def +(right: (String, Any)): Context = new Context(evaluator, env + right)
    def -(right: String): Context = new Context(evaluator, env - right)

    def +=(right: (String, Any)): Unit = env = env + right
    def -=(right: String): Unit = env = env - right
  }

  abstract class FunctionDeclaration(val name: String, val args: List[String]) {
    def run(context: Context): Any
  }
  class FunctionNative(name: String, args: List[String], function: Context => Any) extends FunctionDeclaration(name, args) {
    def run(context: Context): Any = function(context)
  }

//  abstract class FunctionPrototypenUser(name: String, args: List[String]) extends FunctionPrototype(name, args) {

}