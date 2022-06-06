package com.k3rnl.collect.language

import com.k3rnl.collect.Evaluator

trait AST

object AST {

  case class Program(statements: List[AST]) extends AST

  trait Type extends AST {
    override def toString: String = getClass.getSimpleName
  }

  object IntType extends Type
  object StringType extends Type
  object UnitType extends Type

  case class MapType(keyType: Type, valueType: Type) extends Type {
    override def toString: String = s"Map[${keyType.toString}, ${valueType.toString}]"
  }

  trait Statement extends AST

  case class Assignment(name: String, value: Expression) extends Statement

  class RuntimeValue(val value: Any, val typeOf: Type)
  trait Expression extends AST {
    val typeOf: Type
    def run(context: Evaluator.Context): RuntimeValue
  }

  case class Call(name: String, args: List[Expression]) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = {
      context.evaluator.call(name, args, context)
    }

    override val typeOf: Type = UnitType
  }

  case class Constant(value: RuntimeValue) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = value

    override val typeOf: Type = value.typeOf
  }

  case class StringLiteral(value: String) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = new RuntimeValue(value, typeOf)

    override val typeOf: Type = StringType
  }

  case class Variable(name: String, override val typeOf: Type) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = context.env(name)
  }

}



