package com.k3rnl.collect

import com.k3rnl.collect.Database.Row

object Database {
  trait Row {
    def get[R](column: Int): R
    def `()`[R](column: Int): R = get[R](column)
  }
}

trait Database {
  def test(): Unit
  def execute(query: String): Unit
  def query(query: String, callback: Row => Boolean): Unit
  def query(query: String): Traversable[Row] = new Traversable[Row] {
    override def foreach[U](f: Row => U): Unit = {
      Database.this.query(query, (row: Row) => {
        f(row); true
      })
    }
  }
//  def query(query: String)(callback: Row => Unit): Unit = ???
}
