package edu.luc.cs.laufer.cs473.expressions

import ast._
import edu.luc.cs.laufer.cs473.expressions.behaviors.EOL

object PrettyPrinter {

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r)   => -evaluate(r)
    case Plus(l, r)  => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r)   => evaluate(l) / evaluate(r)
    case Mod(l, r)   => evaluate(l) % evaluate(r)
    // case Assignment(l, r)       => 1
    // case Conditional(c, l, r)   => 1
    // case Loop(l, r)             => 1
    // case Block(statements @ _*) => 1
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
    // case Assignment(l, r)       => 1
    // case Conditional(c, l, r)   => 1
    // case Loop(l, r)             => 1
    // case Block(statements @ _*) => 1
  }

  def toPrettyPrinter(prefix: String)(e: Expr): String = e match {
    case Constant(c)            => prefix + c.toString
    case UMinus(r)              => buildUnaryExprString(prefix, "-", toPrettyPrinter(prefix)(r))
    case Plus(l, r)             => buildPrettyExprString(prefix, "+", toPrettyPrinter(prefix)(l), toPrettyPrinter(prefix)(r))
    case Minus(l, r)            => buildPrettyExprString(prefix, "-", toPrettyPrinter(prefix)(l), toPrettyPrinter(prefix)(r))
    case Times(l, r)            => buildPrettyExprString(prefix, "*", toPrettyPrinter(prefix)(l), toPrettyPrinter(prefix)(r))
    case Div(l, r)              => buildPrettyExprString(prefix, "/", toPrettyPrinter(prefix)(l), toPrettyPrinter(prefix)(r))
    case Mod(l, r)              => buildPrettyExprString(prefix, "%", toPrettyPrinter(prefix)(l), toPrettyPrinter(prefix)(r))
    case Variable(v)            => v
    case Assignment(l, r)       => buildPrettyAssignmentString(prefix, "=", toPrettyPrinter(prefix)(l), toPrettyPrinter(prefix)(r)) //Totally Functional
    case Conditional(g, l, r)   => buildPrettyConditionalString(prefix, "if", g, l, r)
    case Loop(l, r)             => buildPrettyLoopString(prefix, "while", toPrettyPrinter(prefix)(l), toPrettyPrinter(prefix)(r))
    case Block(statements @ _*) => buildPrettyBlockString(prefix, statements: _*)
  }

  def toPrettyPrinter(e: Expr): String = toPrettyPrinter("")(e)

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  def buildPrettyExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append("(")
    result.append(leftString)
    result.append(" ")
    result.append(nodeString)
    result.append(" ")
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildPrettyAssignmentString(prefix: String, nodeString: String, l: String, r: String) = {
    val result = new StringBuilder(prefix)
    result.append(l)
    result.append(" ")
    result.append(nodeString)
    result.append(" ")
    result.append(r)
    result.append(";")
    //result.append(EOL)
    result.toString
  }

  def buildPrettyConditionalString(prefix: String, nodeString: String, g: Expr, l: Expr, r: Expr*) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append(" (")
    result.append(toPrettyPrinter(prefix)(g))
    result.append(") ")
    result.append(toPrettyPrinter(prefix)(l))
    r.foreach((block: Expr) => {
      result.append(INDENT)
      result.append("else ")
      result.append(toPrettyPrinter(prefix)(block))
    })
    result.toString
  }

  def buildPrettyLoopString(prefix: String, nodeString: String, l: String, r: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(l)
    result.append(")")
    result.append(r)
    result.toString
  }

  def buildPrettyBlockString(prefix: String, statements: Expr*) = {
    val result = new StringBuilder(prefix)
    result.append("{")
    result.append(statements.map(expr => prefix + EOL + INDENT + toPrettyPrinter(prefix)(expr)).mkString(""))
    result.append(EOL)
    result.append(prefix)
    result.append("}")
    result.append(EOL)
    result.toString
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "
}
