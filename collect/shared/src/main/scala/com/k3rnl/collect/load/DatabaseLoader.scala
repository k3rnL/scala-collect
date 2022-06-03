package com.k3rnl.collect.load
import com.k3rnl.collect.database.Database
import com.k3rnl.collect.transform.Transformer

class DatabaseLoader(writer: Database.Writer) extends Loader {

  override def load(result: Transformer.Result): Unit = {
    writer.write(result.mapping.values)
  }

}
