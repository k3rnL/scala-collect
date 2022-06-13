package com.k3rnl.collect.evaluator

import com.k3rnl.collect.language.lexer.Parser

class CommandLine {
  val evaluator = new Evaluator()

  def run(context: Evaluator.Context): Unit = {
    val line = scala.io.StdIn.readLine()
    Parser.parse(line) match {
      case Parser.Success(result, _) => {
        println(result)
        try {
          run(evaluator.interpret(result.statements, context))
        } catch {
          case e: Exception =>
            println(e)
            run(context)
        }
      }
      case error: Parser.NoSuccess => {
        println(error)
        run(context)
      }
    }
  }
}

object CommandLine extends App {
  val commandLine = new CommandLine()
  commandLine.run(Evaluator.Context(commandLine.evaluator, Map()))
}
