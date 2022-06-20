package com.k3rnl.collect.language.parser

import com.k3rnl.collect.language.{AST, parser}
import com.k3rnl.collect.language.AST.StringType
import com.k3rnl.collect.language.parser.Parser.accept
import com.k3rnl.collect.language.parser.Tokens._

import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Lexer extends RegexParsers {
  override val whiteSpace: Regex = """[ \t]+""".r

  def identifier: Parser[IDENTIFIER] = positioned {
    """[_a-zA-Z](?:([ \w\d]*[\w\d])|[\w\d]*)?+""".r ^^ (string => IDENTIFIER(string))
  }

  def number: Parser[NUMBER] = positioned {
    """-?\d+""".r ^^ (string => NUMBER(string.toInt))
  }

  def string: Parser[STRING] = positioned {
    """"[^"]*"""".r ^^ (string => STRING(string.substring(1, string.length - 1)))
  }

  def comment: Parser[COMMENT] = positioned {
    """#[^\n]*""".r ^^ (_ => Comment)
  }

  def brace_open: Parser[BRACE_OPEN] = positioned {
    """\{""".r ^^ (_ => BraceOpen)
  }

  def brace_close: Parser[BRACE_CLOSE] = positioned{
      """}""".r ^^ (_ => BraceClose)
  }

  def bracket_open: Parser[BRACKET_OPEN] = positioned {
  """\[""".r ^^ (_ => BracketOpen)
  }

  def bracket_close: Parser[BRACKET_CLOSE] = positioned {
      """]""".r ^^ (_ => BracketClose)
  }

  def paren_open: Parser[PAREN_OPEN] = positioned {
      """\(""".r ^^ (_ => ParenOpen)
  }

  def paren_close: Parser[PAREN_CLOSE] = positioned {
      """\)""".r ^^ (_ => ParenClose)
  }

  def comma: Parser[COMMA] = positioned {
      """,""".r ^^ (_ => Comma)
  }

  def dot: Parser[DOT] = positioned {
      """\.""".r ^^ (_ => Dot)
  }

  def equals: Parser[EQUALS] = positioned {
      """=""".r ^^ (_ => Equals)
  }

  def dollar: Parser[DOLLAR] = positioned {
      """\$""".r ^^ (_ => Dollar)
  }

  def eol: Parser[NEWLINE] = positioned {
      """[\r?\n]+""".r ^^ (_ => Newline)
  }

  def arrowRight: Parser[ARROW_RIGHT] = positioned {
      """->""".r ^^ (_ => ArrowRight)
  }

  def token: Parser[Tokens] = (
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

  def tokens: Parser[List[Tokens]] = phrase(rep1(token))

  def apply(input: String): List[Tokens] = parseAll(tokens, input) match {
    case Success(tokens, _) => tokens
    case failure: NoSuccess => println(failure); List()
  }
}

class TokenReader(tokens: Seq[Tokens]) extends Reader[Tokens] {
  def first: Tokens = tokens.head

  def atEnd: Boolean = tokens.isEmpty

  def pos: Position = if (atEnd) NoPosition else first.pos

  def rest: Reader[Tokens] = new TokenReader(tokens.tail)
}
