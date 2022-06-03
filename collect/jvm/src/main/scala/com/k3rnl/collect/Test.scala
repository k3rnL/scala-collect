package com.k3rnl.collect

object Test extends App {

  val db = new ClickhouseDriver()

  db.insert("INSERT INTO test", List(List(1,"2022-05-12 00:00:00", 123)))

}
