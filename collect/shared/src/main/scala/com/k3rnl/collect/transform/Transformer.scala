package com.k3rnl.collect.transform

import com.k3rnl.collect.Evaluator
import com.k3rnl.collect.Evaluator.FunctionNative
import com.k3rnl.collect.extract.Extractor
import com.k3rnl.collect.language.AST
import com.k3rnl.collect.language.AST.{RuntimeValue, StringType}

import scala.collection.immutable
import scala.util.matching.Regex

class TransformerIdentity extends Transformer {
  def transform(record: Extractor.Record, callback: Transformer.Result => Unit): Unit =
    callback(Transformer.Result(record.mapping))
}

class TransformerNewKey(regexp: Regex, key: String, to: String) extends Transformer {
  def transform(record: Extractor.Record, callback: Transformer.Result => Unit): Unit = {
    regexp.findFirstMatchIn(record.mapping(key)) match {
      case Some(m) => callback(Transformer.Result(record.mapping + (to -> m.group(1))))
    }
  }
}

class TransformerEvaluator(evaluator: Evaluator, ast: AST) extends Transformer {
  val args: Seq[String] = List("id", "datetime", "value")
  override def transform(record: Extractor.Record, callback: Transformer.Result => Unit): Unit = {
    evaluator.declaredFunctions += ("output" -> new FunctionNative("output", List("id", "datetime", "value"), context => {
      callback(Transformer.Result(args.map(name => name -> context.env(name).value.toString).toMap))
      null
    }))
    val result = evaluator.interpret(List(ast), new Evaluator.Context(evaluator, record.mapping.map(kv => kv._1 -> new RuntimeValue(kv._2, StringType))))
//    val mapping = result.env.map(x => (x._1, x._2.toString))
//    callback(Transformer.Result(mapping))
  }
}

trait Transformer {
  def transform(record: Extractor.Record, callback: Transformer.Result => Unit): Unit
}

object Transformer {
  case class Result(mapping: Map[String, String])
}
