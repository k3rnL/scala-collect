package com.k3rnl.collect.language.parser

import com.k3rnl.collect.language.{AST, parser}
import com.k3rnl.collect.language.AST.StringType
import com.k3rnl.collect.language.parser.Parser.accept

import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Lexer extends RegexParsers {
  override val whiteSpace: Regex = """[ \t]+""".r

  def identifier: Parser[IDENTIFIER] = """[_a-zA-Z](?:([ \w\d]*[\w\d])|[\w\d]*)?+""".r ^^ (string => IDENTIFIER(string))
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
  def dollar: Parser[DOLLAR.type] = """\$""".r ^^ (_ => DOLLAR)
  def eol: Parser[NEWLINE.type] = """[\r?\n]+""".r ^^ (_ => NEWLINE)
  def arrowRight: Parser[ARROW_RIGHT.type] = """->""".r ^^ (_ => ARROW_RIGHT)

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
      | dollar
      | eol
      | arrowRight
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
