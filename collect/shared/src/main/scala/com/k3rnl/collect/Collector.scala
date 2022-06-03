package com.k3rnl.collect

import com.k3rnl.collect.extract.{CSVExtractor, Extractor}
import com.k3rnl.collect.load.{FileLoader, Loader}
import com.k3rnl.collect.transform.{Transformer, TransformerIdentity, TransformerNewKey}

class Collector(extractor: Extractor, transformer: Transformer, loader: Loader) {
  def run(): Unit = {
    // measure time
    val startTime = System.currentTimeMillis()
    extractor.extract(transformer.transform(_, loader.load))
    val endTime = System.currentTimeMillis()
    println("Time elapsed: " + (endTime - startTime) + "ms")
  }
}

object Collector {
  val test = new Collector(
    new CSVExtractor("/Users/edaniel/IdeaProjects/test-collect/test_files/500000 Sales Records.csv"),
    new TransformerNewKey("(\\w)\\w+".r, "Region", "TEST"),
    new FileLoader("/Users/edaniel/IdeaProjects/test-collect/test_files/output.csv")
  )
}