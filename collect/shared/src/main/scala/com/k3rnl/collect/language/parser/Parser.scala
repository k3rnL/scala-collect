package com.k3rnl.collect.language.parser

import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.StringType
import com.k3rnl.collect.language.parser.Tokens._

import scala.util.parsing.combinator.Parsers

object Parser extends Parsers {
  override type Elem = Tokens

  def parse(tokens: Seq[Tokens]): ParseResult[AST.Program] = {
    val reader = new TokenReader(tokens.filterNot(Comment == _))
    println(tokens)
    program(reader)
  }

  def parse(input: String): ParseResult[AST.Program] = parse(Lexer(input))

  def stringLiteral: Parser[AST.StringLiteral] = accept("string literal", {
    case STRING(value) => AST.StringLiteral(value)
  })
  def numberLiteral: Parser[AST.Constant] = accept("number literal", {
    case NUMBER(value) => AST.StringLiteral(value.toString)
  })
  def constant: Parser[AST.Constant] = stringLiteral | numberLiteral

  def elements: Parser[List[AST.Expression]] = expression ~ rep(Comma ~> expression) ^^ {
    case head ~ tail => head :: tail
  }

  def listLiteral: Parser[AST.ListLiteral] = BracketOpen ~> opt(elements) <~ BracketClose ^^ {
    case Some(list) => AST.ListLiteral(list)
    case None => AST.ListLiteral(List())
  }

  def tupleLiteral: Parser[AST.TupleLiteral] = (stringLiteral | identifier) ~ ArrowRight ~ expression ^^ {
    case name ~ _ ~ value => AST.TupleLiteral(name, value)
  }

  def mapLiteral: Parser[AST.MapLiteral] = BraceOpen ~> opt(repsep(tupleLiteral, Comma)) <~ BraceClose ^^ {
    case Some(list) => AST.MapLiteral(list)
    case None => AST.MapLiteral(List())
  }

  def literals: Parser[AST.Expression] = tupleLiteral | constant | listLiteral | mapLiteral

  def assignment: Parser[AST.Assignment] = (identifier ~ Equals ~ expression) ^^ {
    case variable ~ _ ~ expression => AST.Assignment(variable, expression)
  }

  def simpleIdentifier: Parser[AST.Variable] = accept("identifier", { case IDENTIFIER(name) => AST.Variable(Nil, name, StringType) })
  def qualifiedIdentifier: Parser[AST.Variable] = simpleIdentifier ~ Dot ~ repsep(simpleIdentifier, Dot) ^^ {
    case first ~ _ ~ Nil => AST.Variable(Nil, first.name, StringType)
    case first ~ _ ~ path => AST.Variable(first.name +: path.drop(1).map(_.name), path.last.name, StringType)
  }
  def identifier: Parser[AST.Variable] = qualifiedIdentifier | simpleIdentifier

  def valueMatch: Parser[AST.Expression] = Dollar ~> stringLiteral ^^ {
    string => AST.Call("value", List(AST.Call("match", List(AST.StringLiteral(string.string)))))
  }
  def mapMatch: Parser[AST.Expression] = Dollar ~> BraceOpen ~> stringLiteral <~ BraceClose ^^ {
    string => AST.Call("find", List(AST.StringLiteral(string.string)))
  }
  def matching: Parser[AST.Expression] = mapMatch | valueMatch

  def expressionTail: Parser[AST.Expression] = BracketOpen ~> expression <~ BracketClose
  def expression: Parser[AST.Expression] = (literals | matching | call | identifier | constant) ~ expressionTail.* ^^ {
    case expression ~ Nil => expression
    case expression ~ tail => tail.foldLeft(expression)((expression, index) => AST.Dereference(expression, index))
  }

  def statement: Parser[AST.Statement] = (Newline.* ~> (assignment | expression) <~ Newline.*) | failure("Unexpected character")

  def call: Parser[AST.Call] = identifier ~ ParenOpen ~ repsep(expression, Comma) ~ ParenClose ^^ {
     case function ~ _ ~ arguments ~ _ => AST.Call(function.name, arguments)
  }

  def program: Parser[AST.Program] = phrase(rep(statement)) ^^ {AST.Program(_)}

}

object Test extends App {
  val result = Parser.parse(
    """
      |a = 123
      |print(a)
      |print($"(a=\d).*")
      |""".stripMargin)

  result match {
    case Parser.Success(program, _) =>
      println("Success")
      println(program)
    case e: Parser.NoSuccess => println(e)
  }
}