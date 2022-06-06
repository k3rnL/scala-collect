package com.k3rnl.collect

import com.k3rnl.collect.database.Database
import com.k3rnl.collect.extract.CSVExtractor
import com.k3rnl.collect.language.AST.{IntType, MapType, RuntimeValue, StringType}
import com.k3rnl.collect.language.{AST, BuiltInFunctions}
import com.k3rnl.collect.load.{DatabaseLoader, FileLoader}
import com.k3rnl.collect.transform.TransformerEvaluator

import scala.collection.mutable


object MainCommon {

  def run(): Unit = {
    // measure time
    val startTime = System.currentTimeMillis()

    val ast = AST.Program(List(
      AST.Assignment("a", AST.Constant(new RuntimeValue(Map("a" -> AST.Constant(new RuntimeValue(1, IntType)), "b" -> AST.Constant(new RuntimeValue(2, IntType))), MapType(StringType, IntType)))),
      AST.Call("typeof", List(AST.Variable("a", MapType(StringType, IntType)))),
      AST.Call("print", List(AST.Variable("a", MapType(StringType, IntType)))),
//      AST.Assignment("a", AST.StringLiteral("abc123abc")),
      //    AST.Call("print", List(AST.Variable("a"))),
      AST.Assignment("b", AST.Call("firstMatchingValue", List(AST.Variable("a", StringType), AST.StringLiteral("abc(\\d+)abc")))),
      AST.Call("output", List(AST.Variable("Order ID", StringType), AST.StringLiteral("2022-02-02 00:00:00"), AST.Variable("Total Cost", StringType))),
      //    AST.Call("print", List(AST.Variable("b"))),
    ))

    val evaluator = new Evaluator()
    evaluator.declaredFunctions = BuiltInFunctions.functions

    val db: Database = new ClickhouseDriver()
    val writer = db.insert("insert into test")

    val collector = new Collector(
      new CSVExtractor("/Users/edaniel/IdeaProjects/test-collect/test_files/500000 Sales Records.csv"),
      new TransformerEvaluator(evaluator, ast),
      //    new PrintLoader()
      new DatabaseLoader(writer)
//      new FileLoader("/Users/edaniel/IdeaProjects/test-collect/test_files/output.csv")
    )

    //  var result = mutable.ListBuffer[Int]()

//    val db: Database = new ClickhouseDriver()
//    val result = db.query("SELECT * from system.numbers limit 10000000")
//      .toList

//    println(result.size)

    collector.run()
    writer.close()
    println("Done")

    val endTime = System.currentTimeMillis()
    println("Time: " + (endTime - startTime) + "ms")
  }
}