package com.k3rnl.collect.language.parser

import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.{Program, StringType}
import org.scalatest.flatspec.AnyFlatSpec

class ParserTest extends AnyFlatSpec {

  behavior of "Parser"

  it should "parse an empty map" in {
    val ast = Parser.parse("""{}""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.MapLiteral(List())
    )))
  }

  it should "parse a map of one element" in {
    val ast = Parser.parse("""{"a" -> 123}""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.MapLiteral(List(
        AST.TupleLiteral(AST.StringLiteral("a"), AST.StringLiteral(123.toString))
      ))
    )))
  }

  it should "parse a map of 3 element" in {
    val ast = Parser.parse("""{"a" -> 123, "b" -> 123, "c" -> 123}""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.MapLiteral(List(
        AST.TupleLiteral(AST.StringLiteral("a"), AST.StringLiteral(123.toString)),
        AST.TupleLiteral(AST.StringLiteral("b"), AST.StringLiteral(123.toString)),
        AST.TupleLiteral(AST.StringLiteral("c"), AST.StringLiteral(123.toString))
      ))
    )))
  }

  it should "parse a tuple" in {
    val ast = Parser.parse(""""a" -> 123""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
        AST.TupleLiteral(AST.StringLiteral("a"), AST.StringLiteral(123.toString))
      )
    ))
  }

  it should "parse an empty list" in {
    val ast = Parser.parse("""[]""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.ListLiteral(List())
    )))
  }

  it should "parse a list of one element" in {
    val ast = Parser.parse("""[123]""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.ListLiteral(List(
        AST.StringLiteral(123.toString)
      ))
    )))
  }

  it should "parse a list of three elements" in {
    val ast = Parser.parse("""[123, "a" -> "123", {}]""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.ListLiteral(List(
        AST.StringLiteral(123.toString),
        AST.TupleLiteral(AST.StringLiteral("a"), AST.StringLiteral("123")),
        AST.MapLiteral(List())
      ))
    )))
  }

  it should "parse a function call with no arguments" in {
    val ast = Parser.parse("""fn()""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.Call("fn", List())
    )))
  }

  it should "parse a function call with one argument" in {
    val ast = Parser.parse("""fn(123)""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.Call("fn", List(AST.StringLiteral(123.toString)))
    )))
  }

  it should "parse a function call with three arguments" in {
    val ast = Parser.parse("""fn(123, "a" -> "123", {})""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.Call("fn", List(
        AST.StringLiteral(123.toString),
        AST.TupleLiteral(AST.StringLiteral("a"), AST.StringLiteral("123")),
        AST.MapLiteral(List())
      ))
    )))
  }

  it should "parse an assigment without path of a string" in {
    val ast = Parser.parse("""a = 123""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.Assignment(AST.Variable(Seq(), "a", StringType), AST.StringLiteral(123.toString))
    )))
  }

  it should "parse an assignment with one path of a string" in {
    val ast = Parser.parse("""a.b = 123""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.Assignment(AST.Variable(Seq("a"), "b", StringType), AST.StringLiteral(123.toString))
    )))
  }

  it should "parse an assignment with three paths of a string" in {
    val ast = Parser.parse("""a.b.c = 123""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    assert(ast == Program(List(
      AST.Assignment(AST.Variable(Seq("a", "b"), "c", StringType), AST.StringLiteral(123.toString))
    )))
  }

  it should "parse an assignment of simple path of a complex expression" in {
    val ast = Parser.parse("""a.b = fn([1, 2, {}, [{}, 1, "a" -> "123"]])""") match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }

    assert(ast == Program(List(
      AST.Assignment(AST.Variable(Seq("a"), "b", StringType),
        AST.Call("fn", List(
          AST.ListLiteral(List(
            AST.StringLiteral(1.toString),
            AST.StringLiteral(2.toString),
            AST.MapLiteral(List()),
            AST.ListLiteral(List(
              AST.MapLiteral(List()),
              AST.StringLiteral(1.toString),
              AST.TupleLiteral(AST.StringLiteral("a"), AST.StringLiteral("123"))
            ))
          ))
        ))
      )
    )))
  }

}
