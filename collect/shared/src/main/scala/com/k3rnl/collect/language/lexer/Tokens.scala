package com.k3rnl.collect.language.lexer

sealed trait Token
case class IDENTIFIER(name: String) extends Token
case class NUMBER(number: Int) extends Token
case class STRING(content: String) extends Token
case object COMMENT extends Token
case object BRACE_OPEN extends Token
case object BRACE_CLOSE extends Token
case object BRACKET_OPEN extends Token
case object BRACKET_CLOSE extends Token
case object PAREN_OPEN extends Token
case object PAREN_CLOSE extends Token
case object DOT extends Token
case object COMMA extends Token
case object EQUALS extends Token
case object NEWLINE extends Token
