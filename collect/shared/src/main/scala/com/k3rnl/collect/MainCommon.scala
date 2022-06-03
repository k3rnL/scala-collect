package com.k3rnl.collect

import com.k3rnl.collect.Database.Row
import com.k3rnl.collect.extract.CSVExtractor
import com.k3rnl.collect.language.{AST, BuiltInFunctions}
import com.k3rnl.collect.load.FileLoader
import com.k3rnl.collect.transform.TransformerEvaluator

import scala.collection.mutable


object MainCommon {

  def run(): Unit = {
    // measure time
    val startTime = System.currentTimeMillis()

    val ast = AST.Program(List(
      AST.Assignment("a", AST.Constant("abc123abc")),
      //    AST.Call("print", List(AST.Variable("a"))),
      AST.Assignment("b", AST.Call("firstMatchingValue", List(AST.Variable("a"), AST.Constant("abc(\\d+)abc")))),
      //    AST.Call("print", List(AST.Variable("b"))),
    ))

    val evaluator = new Evaluator()
    evaluator.declaredFunctions = BuiltInFunctions.functions

    val collector = new Collector(
      new CSVExtractor("/Users/edaniel/IdeaProjects/test-collect/test_files/500000 Sales Records.csv"),
      new TransformerEvaluator(evaluator, ast),
      //    new PrintLoader()
      new FileLoader("/Users/edaniel/IdeaProjects/test-collect/test_files/output.csv")
    )

    //  var result = mutable.ListBuffer[Int]()

//    val db: Database = new ClickhouseDriver()
//    val result = db.query("SELECT * from system.numbers limit 10000000")
//      .toList

//    println(result.size)

    collector.run()
    println("Done")

    val endTime = System.currentTimeMillis()
    println("Time: " + (endTime - startTime) + "ms")
  }
}