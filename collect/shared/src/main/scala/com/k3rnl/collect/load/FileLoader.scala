package com.k3rnl.collect.load

import com.k3rnl.collect.transform.Transformer

import java.io.{BufferedOutputStream, OutputStreamWriter}

class FileLoader(file: String) extends Loader {
  val out = new OutputStreamWriter(new BufferedOutputStream(new java.io.FileOutputStream(file)))

  def load(result: Transformer.Result): Unit = {
    result.mapping.foreach(e => out.write(s"${e._1}=${e._2}\n"))
  }

  def close(): Unit = {
    out.close()
  }
}


