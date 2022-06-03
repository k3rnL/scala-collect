package com.k3rnl.collect

import com.clickhouse.client.{ClickHouseClient, ClickHouseFormat, ClickHouseNode, ClickHouseProtocol, ClickHouseRequest, ClickHouseResponse}
import com.k3rnl.collect.Database.Row

import scala.collection.JavaConverters._

class ClickhouseDriver extends Database {
  val preferredProtocol = ClickHouseProtocol.HTTP
  val client = ClickHouseClient.newInstance(preferredProtocol)
  val server = ClickHouseNode.builder.port(preferredProtocol).build



  def test(): Unit = {
    // only HTTP and gRPC are supported at this point// only HTTP and gRPC are supported at this point

    val preferredProtocol = ClickHouseProtocol.HTTP
    // you'll have to parse response manually if use different format
    val preferredFormat = ClickHouseFormat.RowBinaryWithNamesAndTypes

    // connect to localhost, use default port of the preferred protocol
    val server = ClickHouseNode.builder.port(preferredProtocol).build

    try {
      val client = ClickHouseClient.newInstance(preferredProtocol)
      val response = client.connect(server).query("SELECT * FROM system.numbers LIMIT 10").asInstanceOf[ClickHouseRequest[_]].executeAndWait()
      try { // or resp.stream() if you prefer stream API
        for (r <- response.records().asScala) {
          println(r.getValue(0))
          val num = r.getValue(0).asInteger
          val str = r.getValue(0).asString
        }
        val summary = response.getSummary
        val totalRows = summary.getTotalRowsToRead
      } finally {
        if (client != null) client.close()
        if (response != null) response.close()
      }
    }
  }

  override def execute(query: String): Unit = {
    val response = client.connect(server).query(query).asInstanceOf[ClickHouseRequest[_]].executeAndWait()
    response.close()
  }

  override def query(query: String, callback: Database.Row => Boolean): Unit = {
    val response = client.connect(server).query(query).asInstanceOf[ClickHouseRequest[_]].executeAndWait()
    try {
      response.records().asScala.foreach(row => callback(new Row {
        override def get[R](column: Int): R = row.getValue(column).asObject().asInstanceOf[R]
      }))
    } finally {
      if (response != null) response.close()
    }
  }
}

object Test extends App {
  // measure time
  val start = System.currentTimeMillis()
  val client = new ClickhouseDriver()

  val r = client.query("select * from system.numbers limit 10000000")
    .toList

  println(r.size)

  val end = System.currentTimeMillis()
  println(s"Time: ${end - start}")
}