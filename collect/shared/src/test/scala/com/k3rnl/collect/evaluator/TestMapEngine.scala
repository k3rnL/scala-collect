package com.k3rnl.collect.evaluator

import com.k3rnl.collect.language.AST.{RuntimeValue, StringType}
import com.k3rnl.collect.evaluator.MapEngine._
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
    println(map)
    assert(map.get(Seq("a"), "b").get.value == "Test")
  }

  "MapEngine" should "assign value at third level" in {
    var map = empty

    map = map.assign(Seq("a", "b"), "c", RuntimeValue("Test", StringType))
    println(map)
    assert(map.get(Seq("a", "b"), "c").get.value == "Test")
  }

}
