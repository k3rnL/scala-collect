package com.k3rnl.collect.load

import com.k3rnl.collect.transform.Transformer

class PrintLoader extends Loader {
  def load(result: Transformer.Result): Unit = {
    result.mapping.foreach(e => println(s"${e._1}=${e._2}"))
    println("-------------------------------")
  }
}