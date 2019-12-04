package edu.luc.cs.laufer.cs473.expressions

import scala.collection.mutable.Map
import behaviors._

object CombinatorCalculator extends App {

  var repMap = scala.collection.mutable.Map.empty[String, Value] //takes String and value. X is the key and value is number 3

  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.topLevel, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      val expr = result.get
      println("The parsed expression is: ")
      println(toFormattedString(expr))
      println("It evaluates to " + evaluate(repMap, expr))
    }
  }

  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines foreach { line =>
      processExpr(line)
      print("Enter infix expression: ")
    }
  }
}
