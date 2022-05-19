package com.k3rnl.collect.extract

import java.io.{BufferedReader, FileReader}
import java.util.function.Function

import scala.collection.JavaConverters._

class CSVExtractor(file: String) extends Extractor {

  def splitLine(line: String): IndexedSeq[String] = {
    var inQuote = false
    var last = 0
    val result = new scala.collection.mutable.ArrayBuffer[String]
    for (i <- 0 until line.length) {
      if (line(i) == '"') inQuote = !inQuote
      if (line(i) == ',' && !inQuote) {
        val value = line.substring(last, i)
        last = i + 1
        result += value
      }
      if (i == line.length - 1) {
        result += line.substring(last, i + 1)
      }
    }

    result.toIndexedSeq
  }

  def extract(callback: Extractor.Record => Unit): Unit = {
    var header: Option[IndexedSeq[String]] = None

    // read file scala
    scala.io.Source.fromFile(file).getLines().foreach { line =>
      if (header.isEmpty) {
        header = Some(splitLine(line))
      } else {
        callback(Extractor.Record(file, header.get.zip(splitLine(line)).toMap))
      }
    }

  }
}

trait Extractor {
  def extract(callback: Extractor.Record => Unit): Unit
}

object Extractor {
  case class Record(name: String, mapping: Map[String, String])
}
