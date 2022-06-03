package com.k3rnl.collect

import com.k3rnl.collect.ClickhouseDriver.Type.Type
import com.k3rnl.collect.database.Database
import com.k3rnl.collect.database.Database.{AnyRow, Row}

import scala.scalanative.runtime.{Intrinsics, fromRawPtr, toRawPtr}
import scala.scalanative.unsafe._
import scala.scalanative.unsigned.UnsignedRichLong

@link("clickhouse-cpp-lib")
@extern
object clickhouse_driver {
  def getClient: Ptr[Byte] = extern

  def execute(client: Ptr[Byte], query: CString): Unit = extern
  def query(client: Ptr[Byte], query: CString, context: Ptr[Unit], callback: CFuncPtr2[Ptr[Byte], Ptr[Unit], Boolean]): Unit = extern

  def block_getRowCount(block: Ptr[Byte]): CSize = extern
  def block_getColumnCount(block: Ptr[Byte]): CSize = extern
  def block_getColumn(block: Ptr[Byte], index: CSize): Ptr[Byte] = extern

  def column_type(column: Ptr[Byte]): CShort = extern
  def column_at(column: Ptr[Byte], index: CSize): Ptr[Byte] = extern
}

object ClickhouseDriver {
  class ClickhouseRow(block: Block, index: Long) extends Row {
    override def get[R](column: CInt): R = block.getColumn(column).at(index).asInstanceOf[R]
  }

  class Block(ptr: Ptr[Byte]) {
    def rowCount: Long = clickhouse_driver.block_getRowCount(ptr).toLong

    def columnCount: Long = clickhouse_driver.block_getColumnCount(ptr).toLong

    def getColumn(index: Long): Column = new Column(clickhouse_driver.block_getColumn(ptr, index.toULong))

    def row(index: Long): Row = new ClickhouseRow(this, index)
  }

  class Column(ptr: Ptr[Byte]) {
    def at(index: Long): Any = {
      val value = clickhouse_driver.column_at(ptr, index.toULong)
      `type` match {
        case Type.Int8 => !value
        case Type.Int16 => !value.asInstanceOf[Ptr[Short]]
        case Type.Int32 => !value.asInstanceOf[Ptr[Int]]
        case Type.Int64 => !value.asInstanceOf[Ptr[Long]]
        case Type.String => fromCString(value)
        case Type.DateTime => !value.asInstanceOf[Ptr[Long]]
        case Type.UInt8 => !value
        case Type.UInt16 => !value.asInstanceOf[Ptr[Short]]
        case Type.UInt32 => !value.asInstanceOf[Ptr[Int]]
        case Type.UInt64 => !value.asInstanceOf[Ptr[Long]]
        //        case Type.Float32 => value.toFloat
        //        case Type.Float64 => value.toDouble
        //        case Type.String => value.toString
        //        case Type.Date => value.toLong
        //        case Type.DateTime => value.toLong
        //        case Type.UUID => value.toString
        //        case Type.Enum8 => value.toByte
        //        case Type.Enum16 => value.toShort
        case _ =>
          println("Unknown type " + `type`)
          throw new Exception("Unknown type")
      }
    }

    def `type`: Type = Type.from(clickhouse_driver.column_type(ptr))
  }

  object Type extends Enumeration {
    type Type = Value
    val
    Void,
    Int8, Int16, Int32, Int64, UInt8,
    UInt16, UInt32, UInt64,
    Float32, Float64,
    String, FixedString,
    DateTime, Date,
    Array,
    Nullable,
    Tuple,
    Enum8, Enum16,
    UUID = Value

    def from(value: CShort): Type = values.find(_.id == value).get
  }

}

class ClickhouseDriver extends Database {

  val client: Ptr[Byte] = clickhouse_driver.getClient

  def test(): Unit = println(clickhouse_driver.getClient)

  def execute(query: String): Unit = {
    Zone { implicit z =>
      clickhouse_driver.execute(client, toCString(query))
    }
  }

  def query(query: String, callback: Row => Boolean): Unit = {
    val context = fromRawPtr[Unit](Intrinsics.castObjectToRawPtr(callback))
    Zone { implicit z =>
      clickhouse_driver.query(client, toCString(query), context, (ptr: Ptr[Byte], context: Ptr[Unit]) => {
        val cb = Intrinsics.castRawPtrToObject(toRawPtr(context)).asInstanceOf[Row => Boolean]
        processBlock(ptr, cb)
      })
    }
  }

  def processBlock(ptr: Ptr[Byte], callback: Row => Boolean): Boolean = {
    val block = new ClickhouseDriver.Block(ptr)
    Iterator.iterate(0)(_ + 1).takeWhile(i => i < block.rowCount).forall(i => {
      callback(block.row(i))
      true
    })
  }

  override def insert(query: String, data: Traversable[AnyRow]): Unit = ???

  override def insert(query: String): Writer = ???
}

