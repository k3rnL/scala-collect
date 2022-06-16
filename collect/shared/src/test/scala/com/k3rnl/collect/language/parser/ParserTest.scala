package com.k3rnl.collect.language.parser

import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.Program
import org.scalatest.flatspec.AnyFlatSpec

class ParserTest extends AnyFlatSpec {

  behavior of "Parser"

  it should "parse a simple string" in {
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

}
