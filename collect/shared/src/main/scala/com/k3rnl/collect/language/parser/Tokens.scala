package com.k3rnl.collect.language.parser

import scala.util.parsing.input.Positional


object Tokens {
  def Comment: COMMENT = COMMENT()
  def BraceOpen: BRACE_OPEN = BRACE_OPEN()
  def BraceClose: BRACE_CLOSE = BRACE_CLOSE()
  def BracketOpen: BRACKET_OPEN = BRACKET_OPEN()
  def BracketClose: BRACKET_CLOSE = BRACKET_CLOSE()
  def ParenOpen: PAREN_OPEN = PAREN_OPEN()
  def ParenClose: PAREN_CLOSE = PAREN_CLOSE()
  def Dot: DOT = DOT()
  def Comma: COMMA = COMMA()
  def Equals: EQUALS = EQUALS()
  def Dollar: DOLLAR = DOLLAR()
  def Newline: NEWLINE = NEWLINE()
  def ArrowRight: ARROW_RIGHT = ARROW_RIGHT()
}

sealed trait Tokens extends Positional

case class IDENTIFIER(name: String) extends Tokens
case class NUMBER(number: Int) extends Tokens
case class STRING(content: String) extends Tokens
case class COMMENT() extends Tokens
case class BRACE_OPEN() extends Tokens
case class BRACE_CLOSE() extends Tokens
case class BRACKET_OPEN() extends Tokens
case class BRACKET_CLOSE() extends Tokens
case class PAREN_OPEN() extends Tokens
case class PAREN_CLOSE() extends Tokens
case class DOT() extends Tokens
case class COMMA() extends Tokens
case class EQUALS() extends Tokens
case class DOLLAR() extends Tokens
case class NEWLINE() extends Tokens
case class ARROW_RIGHT() extends Tokens