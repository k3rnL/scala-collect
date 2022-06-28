package com.k3rnl.collect.evaluator

import com.k3rnl.collect.evaluator.DurationExtensions.HumanReadableExtension
import com.k3rnl.collect.language.parser.Parser

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

class CommandLine {
  val evaluator = new Evaluator()

  def run(): Unit = {
    var context = Evaluator.Context(evaluator, Map())
    while (true) {
      print(">>> ")
      val line = scala.io.StdIn.readLine()
      if (line == null) {
        println("Bye!")
        return
      }
      Parser.parse(line) match {
        case Parser.Success(result, _) =>
          println(result)
          try {
            val now = System.nanoTime()
            context = evaluator.interpret(result.statements, context)
            println("Time: " + (System.nanoTime() - now).nanos.toHumanReadable)
            println("Time: " + (System.nanoTime() - now).nanos)
          } catch {
            case e: Exception =>
              println(e)
          }
        case error: Parser.NoSuccess =>
          println(error)
      }
    }
  }
}

object CommandLine extends App {
  new CommandLine().run()
}

object DurationExtensions {
  implicit class HumanReadableExtension(duration: Duration) {
    final def toHumanReadable: String = {
      val units = Seq(TimeUnit.DAYS, TimeUnit.HOURS, TimeUnit.MINUTES, TimeUnit.SECONDS,
        TimeUnit.MILLISECONDS, TimeUnit.NANOSECONDS)

      val timeStrings = units
        .foldLeft((Seq.empty[String], duration.toMillis))({ case ((humanReadable, rest), unit) =>
          val name = unit.toString.toLowerCase()
          val result = unit.convert(rest, TimeUnit.MILLISECONDS)
          val diff = rest - TimeUnit.MILLISECONDS.convert(result, unit)
          val str = result match {
            case 0    => humanReadable
            case 1    => humanReadable :+ s"1 ${name.init}" // Drop last 's'
            case more => humanReadable :+ s"$more $name"
          }
          (str, diff)
        })._1

      timeStrings.size match {
        case 0 => ""
        case 1 => timeStrings.head
        case _ => timeStrings.init.mkString(", ") + " and " + timeStrings.last
      }
    }
  }
}