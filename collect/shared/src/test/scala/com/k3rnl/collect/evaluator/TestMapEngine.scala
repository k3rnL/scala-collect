package com.k3rnl.collect.evaluator

import com.k3rnl.collect.language.AST.{RuntimeValue, StringType}
import com.k3rnl.collect.evaluator.MapEngine._
import com.k3rnl.collect.language.parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

class TestMapEngine extends AnyFlatSpec {

  val empty = new MapEngine(Map[String, RuntimeValue]())

  "MapEngine" should "assign value at root level" in {
    var map = empty

    map = map.assign("a", RuntimeValue("Test", StringType))
    assert(map("a").value == "Test")
  }

  "MapEngine" should "assign value at second level" in {
    var map = empty

    map = map.assign(Seq("a"), "b", RuntimeValue("Test", StringType))
    assert(map.get(Seq("a"), "b").get.value == "Test")
  }

  "MapEngine" should "assign value at third level" in {
    var map = empty

    map = map.assign(Seq("a", "b"), "c", RuntimeValue("Test", StringType))
    assert(map.get(Seq("a", "b"), "c").get.value == "Test")
  }

  "MapEngine" must "access b when declared from map" in {
    val ast = Parser.parse(
      """a = {"b" -> 1}""".stripMargin) match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    val result = new Evaluator().interpret(ast.statements)
    assert(new MapEngine(result.env).get(List("a"), "b").get.value == 1.toString)
  }

  "MapEngine" must "access b when compiled from parser" in {
    val ast = Parser.parse(
      """a.b = 1""".stripMargin) match {
      case Parser.Success(result, _) => result
      case e: Parser.NoSuccess => fail(e.toString)
    }
    val result = new Evaluator().interpret(ast.statements)
    assert(new MapEngine(result.env).get(List("a"), "b").get.value == 1.toString)
  }

}
