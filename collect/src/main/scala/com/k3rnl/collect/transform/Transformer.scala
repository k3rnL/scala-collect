package com.k3rnl.collect.transform

import com.k3rnl.collect.Evaluator
import com.k3rnl.collect.extract.Extractor
import com.k3rnl.collect.language.AST

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
  override def transform(record: Extractor.Record, callback: Transformer.Result => Unit): Unit = {
    val result = evaluator.interpret(List(ast), new Evaluator.Context(evaluator, record.mapping))
    val mapping = result.env.map(x => (x._1, x._2.toString))
    callback(Transformer.Result(mapping))
  }
}

trait Transformer {
  def transform(record: Extractor.Record, callback: Transformer.Result => Unit): Unit
}

object Transformer {
  case class Result(mapping: Map[String, String])
}
