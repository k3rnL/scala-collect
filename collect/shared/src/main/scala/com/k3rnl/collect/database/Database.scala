package com.k3rnl.collect.database

import com.k3rnl.collect.database.Database.{AnyRow, Row, Writer}

object Database {
  trait Row extends Traversable[Any] {
    def get[R](column: Int): R
    def `()`[R](column: Int): R = get[R](column)
  }

  type AnyRow = Traversable[Any]

  trait Writer extends AutoCloseable{
    def write(data: AnyRow): Unit
    def close(): Unit
  }
}

trait Database {
  def execute(query: String): Unit
  def query(query: String, callback: Row => Boolean): Unit
  def query(query: String): Traversable[Row] = new Traversable[Row] {
    override def foreach[U](f: Row => U): Unit = {
      Database.this.query(query, (row: Row) => {
        f(row); true
      })
    }
  }
  def insert(query: String, data: Traversable[AnyRow]): Unit
  def insert(query: String): Writer
}
