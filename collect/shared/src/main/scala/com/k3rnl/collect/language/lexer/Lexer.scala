package com.k3rnl.collect.language.lexer

import com.k3rnl.collect.language.{AST, lexer}
import com.k3rnl.collect.language.AST.StringType

import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Lexer extends RegexParsers {
  def identifier: Parser[IDENTIFIER] = """[a-zA-Z][a-zA-Z\d ]+[a-zA-Z]""".r ^^ (string => IDENTIFIER(string))
  def number: Parser[NUMBER] = """-?\d+""".r ^^ (string => NUMBER(string.toInt))
  def string: Parser[STRING] = """"[^"]*"""".r ^^ (string => STRING(string.substring(1, string.length - 1)))
  def comment: Parser[COMMENT.type] = """#[^\n]*""".r ^^ (_ => COMMENT)
  def brace_open: Parser[BRACE_OPEN.type] = """\{""".r ^^ (_ => BRACE_OPEN)
  def brace_close: Parser[BRACE_CLOSE.type] = """}""".r ^^ (_ => BRACE_CLOSE)
  def bracket_open: Parser[BRACKET_OPEN.type] = """\[""".r ^^ (_ => BRACKET_OPEN)
  def bracket_close: Parser[BRACKET_CLOSE.type] = """]""".r ^^ (_ => BRACKET_CLOSE)
  def paren_open: Parser[PAREN_OPEN.type] = """\(""".r ^^ (_ => PAREN_OPEN)
  def paren_close: Parser[PAREN_CLOSE.type] = """\)""".r ^^ (_ => PAREN_CLOSE)
  def comma: Parser[COMMA.type] = """,""".r ^^ (_ => COMMA)
  def dot: Parser[DOT.type] = """\.""".r ^^ (_ => DOT)
  def equals: Parser[EQUALS.type] = """=""".r ^^ (_ => EQUALS)
  def newline: Parser[NEWLINE.type] = """\n""".r ^^ (_ => NEWLINE)

  def token: Parser[Token] = identifier | number | string | comment | brace_open | brace_close | bracket_open | bracket_close | paren_open | paren_close | comma | dot | equals | newline
  def tokens: Parser[List[Token]] = phrase(rep1(token))

  def apply(input: String): List[Token] = parseAll(tokens, input) match {
    case Success(tokens, _) => tokens
    case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
  }
}

class TokenReader[T](tokens: Seq[T]) extends Reader[T] {
  def first: T = tokens.head
  def atEnd: Boolean = tokens.isEmpty
  def pos: Position = NoPosition
  def rest: Reader[T] = new TokenReader(tokens.tail)
}

object Parser extends Parsers {
  override type Elem = Token

  def parse(tokens: Seq[Token]): ParseResult[AST.Program] = {
    val reader = new TokenReader(tokens.filterNot(_ == COMMENT))
    program(reader)
  }

  def parse(input: String): ParseResult[AST.Program] = parse(Lexer(input))

  def constant: Parser[AST.Constant] = accept("constant", {
    case NUMBER(value) => AST.StringLiteral(value.toString)
    case STRING(value) => AST.StringLiteral(value)
  })

  def assignment: Parser[AST.Assignment] = (identifier ~ EQUALS ~ expression) ^^ {
    case variable ~ _ ~ expression => AST.Assignment(variable, expression)
  }

  def simpleIdentifier: Parser[AST.Variable] = accept("identifier", { case IDENTIFIER(name) => AST.Variable(Nil, name, StringType) })
  def qualifiedIdentifier: Parser[AST.Variable] = simpleIdentifier ~ DOT ~ repsep(simpleIdentifier, DOT) ^^ {
    case first ~ _ ~ Nil => AST.Variable(Nil, first.name, StringType)
    case first ~ _ ~ path => AST.Variable(first.name +: path.drop(1).map(_.name), path.last.name, StringType)
  }
  def identifier: Parser[AST.Variable] = qualifiedIdentifier | simpleIdentifier

  def expression: Parser[AST.Expression] = call | identifier | constant

  def statement: Parser[AST.Statement] = assignment | expression

  def call: Parser[AST.Call] = identifier ~ PAREN_OPEN ~ repsep(expression, COMMA) ~ PAREN_CLOSE ^^ {
     case function ~ _ ~ arguments ~ _ => AST.Call(function.name, arguments)
  }

  def program: Parser[AST.Program] = rep(statement) ^^ (statements => AST.Program(statements))

}

object Test extends App {
  val result = Parser.parse(
    """output(Order Id, "2022-02-02 00:00:00", Total Cost)
      |""".stripMargin)

  println(result)
}