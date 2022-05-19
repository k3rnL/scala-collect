package com.k3rnl.collect.language

import com.k3rnl.collect.Evaluator

trait AST

object AST {

  case class Program(statements: List[AST]) extends AST

  trait Statement extends AST

  case class Assignment(name: String, value: Expression) extends Statement

  trait Expression extends AST {
    def run(context: Evaluator.Context): Any
  }

  case class Call(name: String, args: List[Expression]) extends Expression {
    override def run(context: Evaluator.Context): Any = {
      context.evaluator.call(name, args, context)
    }
  }

  case class Constant(value: Any) extends Expression {
    override def run(context: Evaluator.Context): Any = value
  }

  case class Variable(name: String) extends Expression {
    override def run(context: Evaluator.Context): Any = context.env(name)
  }

}



