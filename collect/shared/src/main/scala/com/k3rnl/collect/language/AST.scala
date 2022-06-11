package com.k3rnl.collect.language

import com.k3rnl.collect.evaluator.Evaluator
import com.k3rnl.collect.evaluator.MapEngine._

trait AST

object AST {

  case class Program(statements: List[AST]) extends AST

  trait Type extends AST {
    override def toString: String = getClass.getSimpleName
  }

  class AnyType extends Type

  case object IntType extends AnyType

  case object StringType extends AnyType

  case object UnitType extends AnyType

  case object MapType extends AnyType

  case object ListType extends AnyType

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

  case class Dereference(value: Expression, index: Expression) extends Expression {
    override val typeOf: Type = new AnyType

    override def run(context: Evaluator.Context): RuntimeValue = {
      val index = this.index.run(context)
      value.run(context) match {
        case RuntimeValue(value, MapType) => value.asInstanceOf[Map[String, RuntimeValue]].get(Seq(), index.value.toString).get
        case RuntimeValue(value, ListType) => value.asInstanceOf[List[RuntimeValue]](index.value.toString.toInt)
        case _ => throw new Exception("Cannot dereference non-map or non-list")
      }
    }
  }

  class Constant(value: RuntimeValue) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = value

    override val typeOf: Type = value.typeOf
  }

  case class StringLiteral(string: String) extends Constant(RuntimeValue(string, StringType))

  case class ListLiteral(list: List[Expression]) extends Expression {
    override def run(context: Evaluator.Context): RuntimeValue = {
      RuntimeValue(list.map(_.run(context)), ListType)
    }

    override val typeOf: Type = ListType
  }

  case class Variable(path: Seq[String] = Nil, name: String, override val typeOf: Type) extends Expression {
    lazy val fullName: String = path match {
      case Nil => name
      case _ => path.mkString(".") + "." + name
    }

    override def run(context: Evaluator.Context): RuntimeValue = context.env.get(path, name) match {
      case Some(value) => value
      case None => throw new Exception(s"Variable $fullName not found")
    }
  }

}



