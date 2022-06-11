package com.k3rnl.collect.language.lexer

import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.StringType

import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Lexer extends RegexParsers {
  override val whiteSpace: Regex = """[ \t]+""".r

  def identifier: Parser[IDENTIFIER] = """[a-zA-Z]([a-zA-Z\d ]+[a-zA-Z])?""".r ^^ (string => IDENTIFIER(string))
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
  def eol: Parser[NEWLINE.type] = """[\r?\n]+""".r ^^ (_ => NEWLINE)

  def token: Parser[Token] = (
    identifier
      | number
      | string
      | comment
      | brace_open
      | brace_close
      | bracket_open
      | bracket_close
      | paren_open
      | paren_close
      | comma
      | dot
      | equals
      | eol
      | failure("Unexpected character")
    )

  def tokens: Parser[List[Token]] = phrase(rep1(token))

  def apply(input: String): List[Token] = parseAll(tokens, input) match {
    case Success(tokens, _) => tokens
    case failure: NoSuccess => println(failure); List()
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
    println(tokens)
    program(reader)
  }

  def parse(input: String): ParseResult[AST.Program] = parse(Lexer(input))

  def constant: Parser[AST.Constant] = accept("constant", {
    case NUMBER(value) => AST.StringLiteral(value.toString)
    case STRING(value) => AST.StringLiteral(value)
  })

  def elements: Parser[List[AST.Expression]] = expression ~ rep(COMMA ~> expression) ^^ {
    case head ~ tail => head :: tail
  }

  def listLiteral: Parser[AST.ListLiteral] = BRACKET_OPEN ~> opt(elements) <~ BRACKET_CLOSE ^^ {
    case Some(list) => AST.ListLiteral(list)
    case None => AST.ListLiteral(List())
  }

  def assignment: Parser[AST.Assignment] = (identifier ~ EQUALS ~ expression) ^^ {
    case variable ~ _ ~ expression => AST.Assignment(variable, expression)
  }

  def simpleIdentifier: Parser[AST.Variable] = accept("identifier", { case IDENTIFIER(name) => AST.Variable(Nil, name, StringType) })
  def qualifiedIdentifier: Parser[AST.Variable] = simpleIdentifier ~ DOT ~ repsep(simpleIdentifier, DOT) ^^ {
    case first ~ _ ~ Nil => AST.Variable(Nil, first.name, StringType)
    case first ~ _ ~ path => AST.Variable(first.name +: path.drop(1).map(_.name), path.last.name, StringType)
  }
  def identifier: Parser[AST.Variable] = qualifiedIdentifier | simpleIdentifier

  def expressionTail: Parser[AST.Expression] = BRACKET_OPEN ~> expression <~ BRACKET_CLOSE
  def expression: Parser[AST.Expression] = (call | identifier | constant | listLiteral) ~ expressionTail.* ^^ {
    case expression ~ Nil => expression
    case expression ~ tail => tail.foldLeft(expression)((expression, index) => AST.Dereference(expression, index))
  }

  def statement: Parser[AST.Statement] = (NEWLINE.* ~> (assignment | expression) <~ NEWLINE.*) | failure("Unexpected character")

  def call: Parser[AST.Call] = identifier ~ PAREN_OPEN ~ repsep(expression, COMMA) ~ PAREN_CLOSE ^^ {
     case function ~ _ ~ arguments ~ _ => AST.Call(function.name, arguments)
  }

  def program: Parser[AST.Program] = rep(statement) ^^ {AST.Program(_)} | failure("No statements found")

}

object Test extends App {
  val result = Parser.parse(
    """
      |
      |a = [[1]][0][0]
      |output(Order Id, "2022-02-02 00:00:00", Total Cost)
      |""".stripMargin)

  result match {
    case Parser.Success(program, _) =>
      println("Success")
      println(program)
    case Parser.NoSuccess(error, _) => println(error)
  }
}