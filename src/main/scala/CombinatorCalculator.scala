package edu.luc.cs.laufer.cs473.expressions

import scala.collection.mutable.Map
import behaviors._
import org.jline.reader.LineReaderBuilder
import org.jline.terminal.TerminalBuilder
import executor.Value
import PrettyPrinter._


object CombinatorCalculator extends App {
  val terminal = TerminalBuilder.terminal
  val reader = LineReaderBuilder.builder.terminal(terminal).build
  val prompt = "Enter infix expression: "
  var memoryBank = scala.collection.mutable.Map.empty[String, Value]
  def processExpr(input: String): Unit = {
    println("Memory: " + memoryBank)
    println()
    println("You entered: " + input)
    println()
    val result = CombinatorParser.parseAll(CombinatorParser.topLevel, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      val expr = result.get
      val pp = result.get
      println("The unparsed expression is: ")
      println(toFormattedString(expr))
      println("")
      println("The parsed expression is: ")
      println(toPrettyPrinter(pp))
      println()
      print("It evaluates to: ")
      println(executor.execute(memoryBank)(expr))
      println()
      println("Memory: " + memoryBank)
      println()
    }
  }
  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    while (true) {
      val in = reader.readLine(prompt)
      processExpr(in)
    }
  }
}