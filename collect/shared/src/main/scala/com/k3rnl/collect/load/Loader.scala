package com.k3rnl.collect.load

import com.k3rnl.collect.transform.Transformer

trait Loader {
  def load(result: Transformer.Result): Unit
}