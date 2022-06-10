package com.k3rnl.collect.language

import com.k3rnl.collect.evaluator.Evaluator
import com.k3rnl.collect.evaluator.MapEngine._

trait AST

object AST {

  case class Program(statements: List[AST]) extends AST

  trait Type extends AST {
    override def toString: String = getClass.getSimpleName
  }

  object AnyType extends Type
  object IntType extends Type
  object StringType extends Type
  object UnitType extends Type

  case object MapType extends Type {
    override def toString: String = s"Map"
  }

  trait Statement extends AST

  case class Assignment(variable: Variable, value: Expression) extends Statement

  case class RuntimeValue(val value: Any, val typeOf: Type) {
    override def toString: String = value.toString
  }
  trait Expression extends Statement {
    val typeOf: Type
    def run(context: Evaluator.Context): RuntimeValue
  }

  case class Call(name: String, args: List[Expression]) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = {
      context.evaluator.call(name, args, context)
    }

    override val typeOf: Type = UnitType
  }

  class Constant(value: RuntimeValue) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = value

    override val typeOf: Type = value.typeOf
  }

  case class StringLiteral(string: String) extends Constant(RuntimeValue(string, StringType))

  case class Variable(path: Seq[String] = Nil, name: String, override val typeOf: Type) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = context.env.get(path, name) match {
      case Some(value) => value
      case None => throw new Exception(s"Variable $name not found")
    }
  }

}



