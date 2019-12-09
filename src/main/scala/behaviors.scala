package edu.luc.cs.laufer.cs473.expressions

import ast._
import scala.collection.mutable.Map
import scala.util.{Try, Success, Failure}

object behaviors {

  sealed trait Value
  case class Number(value: Int) extends Value

  object Value {
    val NULL = Number(0)
  }

  def evaluate(repMap: Map[String, Value], e: Expr): Value = e match {
    case Constant(c) => Number(c)
    case UMinus(r)   => Number(-evaluate(repMap, r).asInstanceOf[Number].value)
    case Plus(l, r)  => Number(evaluate(repMap, l).asInstanceOf[Number].value + evaluate(repMap, r).asInstanceOf[Number].value)
    case Minus(l, r) => Number(evaluate(repMap, l).asInstanceOf[Number].value - evaluate(repMap, r).asInstanceOf[Number].value)
    case Times(l, r) => Number(evaluate(repMap, l).asInstanceOf[Number].value * evaluate(repMap, r).asInstanceOf[Number].value)
    case Div(l, r)   => Number(evaluate(repMap, l).asInstanceOf[Number].value / evaluate(repMap, r).asInstanceOf[Number].value)
    case Mod(l, r)   => Number(evaluate(repMap, l).asInstanceOf[Number].value % evaluate(repMap, r).asInstanceOf[Number].value)
    case Loop(l, r)  => Number(1)

    case Cond(guard, thenBranch, elseBranch) => {
      evaluate(repMap, guard) match {
        case (Value.NULL) => evaluate(repMap, elseBranch)
        case (_)          => evaluate(repMap, thenBranch)
        //case f @ Failure(_)     => f
      }
    }

    case Block(expressions @ _*) => {
      // TODO http://stackoverflow.com/questions/12892701/abort-early-in-a-fold
      val i = expressions.iterator
      var result: Value = Value.NULL
      while (i.hasNext)
        evaluate(repMap, i.next()) match {
          case r => result = r.asInstanceOf[Number]
          //case f@Failure(_) => return f
        }

      result
    }

    case Assign(left, right) => {
      repMap(left.asInstanceOf[Variable].name) = evaluate(repMap, right)
      return null

    }

    case Variable(name) => {
      if (repMap.contains(name)) {
        repMap(name)
        //Number(repMap().asInstanceOf[Number].value) //passes key to value
      } else {
        throw new IllegalArgumentException("Variable does not exist in memory")
      }
    }

    case Loop(l, r) => {
      var catchcond: Int = 0
      while (catchcond == 0) {
        evaluate(repMap, l) match {
          case (v)          => evaluate(repMap, r)
          case (Value.NULL) => return (Value.NULL)

          //case Success(Value.NULL) => return Success(Value.NULL)
          //case Success(v)          => evaluate(memory, r)
          //case f @ Failure(_) => {
          //return f
        }
      }
      return Number(0)
    }
    //Success(Value.NULL)
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
  }

  def height(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c) => prefix + c.toString
    case UMinus(r)   => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r)  => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r) => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r) => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r)   => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r)   => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Loop(l, r)  => buildExprString(prefix, "Loop", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))

    case Cond(l, m, r) => {

      if (r.toString == "Block(List())") {
        buildExprString(prefix, "Conditional", toFormattedString(prefix + INDENT)(m), toFormattedString(prefix + INDENT)(l))
      } else {
        buildTriExprString(prefix, "Conditional", toFormattedString(prefix + INDENT)(m), toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))

      }

    }

    case Block(statements @ _*) => mapToFunction(prefix, statements, "Block")
    case Assign(l, r)           => buildExprString(prefix, "Assignment", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Variable(x)            => prefix + x.toString

  }

  def mapToFunction(prefix: String, e: Seq[Expr], nodeString: String): String = {
    val result = new StringBuilder(prefix)
    val strings = e.map(expr => toFormattedString(prefix)(expr))
    strings.foreach(strings => result.append(strings))
    result.toString
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildTriExprString(prefix: String, nodeString: String, Condition: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(Condition)
    result.append(")Else( ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
