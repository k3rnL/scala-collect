package com.k3rnl.collect

import com.k3rnl.collect.extract.CSVExtractor
import com.k3rnl.collect.language.{AST, BuiltInFunctions}
import com.k3rnl.collect.load.FileLoader
import com.k3rnl.collect.transform.TransformerEvaluator


object Main extends App {
  MainCommon.run()
}
