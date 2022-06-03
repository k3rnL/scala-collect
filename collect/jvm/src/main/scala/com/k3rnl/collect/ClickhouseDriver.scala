package com.k3rnl.collect

import com.clickhouse.client.stream.{CapacityPolicy, NonBlockingPipedOutputStream}
import com.clickhouse.client._
import com.k3rnl.collect.database.Database
import com.k3rnl.collect.database.Database.{AnyRow, Row}

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

        override def foreach[U](f: Any => U): Unit = {
          row.iterator().forEachRemaining(v => f(v.asObject()))
        }

        override def size: Int = row.size()
      }))
    } finally {
      if (response != null) response.close()
    }
  }

  override def insert(query: String, data: Traversable[AnyRow]): Unit = {
    val outputStream = new NonBlockingPipedOutputStream(8096, 8096, 30000,
      CapacityPolicy.fixedCapacity(16), () => {})

    val response = client.connect(server).write().query(query).format(ClickHouseFormat.TSV)
      .data(outputStream.getInputStream())

    data.foreach(row => {
      val str = row.map(v => v.toString).mkString("\t")
      outputStream.write(str.getBytes)
      outputStream.write('\n')
    })

    outputStream.close()

    response.executeAndWait()
  }
}
