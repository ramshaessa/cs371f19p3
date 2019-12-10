package edu.luc.cs.laufer.cs473.expressions
import ast._
import scala.util.{Failure, Success, Try}
import scala.collection.mutable

object behaviors {

  object executor {

    type Instance = mutable.Map[String, Value]
    val instance: Instance = mutable.Map[String, Value]()

    type Store = Instance

    sealed trait Value

    case class Num(value: Int) extends Value

    object Value {
      val NULL = Num(0)
    }

    type Result = Try[Value]

    def lookup(store: Store)(name: String): Result =
      store.get(name).fold {
        Failure(new NoSuchFieldException(name)): Result
      } {
        Success(_)
      }

    def execute(statements: List[Expr]): Try[Value] = {
      val evaluatedStatements = statements.map(execute(_))
      evaluatedStatements.lastOption.getOrElse(Try(Num(0)))
    }

    def execute(expr: Expr): Try[Value] = { Try(execute(instance)(expr).get) }

    def execute(store: Store)(e: Expr): Result = e match {
      case Constant(c) => Success(Num(c))
      case UMinus(r)   => Try(Num(-execute(store)(r).get.asInstanceOf[Num].value))
      case Plus(l, r)  => Try(Num(execute(store)(l).get.asInstanceOf[Num].value + execute(store)(r).get.asInstanceOf[Num].value))
      case Minus(l, r) => Try(Num(execute(store)(l).get.asInstanceOf[Num].value - execute(store)(r).get.asInstanceOf[Num].value))
      case Times(l, r) => Try(Num(execute(store)(l).get.asInstanceOf[Num].value * execute(store)(r).get.asInstanceOf[Num].value))
      case Div(l, r)   => Try(Num(execute(store)(l).get.asInstanceOf[Num].value / execute(store)(r).get.asInstanceOf[Num].value))
      case Mod(l, r)   => Try(Num(execute(store)(l).get.asInstanceOf[Num].value % execute(store)(r).get.asInstanceOf[Num].value))
      case Variable(c) => lookup(store)(c)
      case Assignment(l, r) => Try {
        val Variable(v) = l
        (store)(v) = Num(execute(store)(r).get.asInstanceOf[Num].value)
        Num(0)
      }
      case Block(statements @ _*) => {
        val i = statements.iterator
        var result: Num = Value.NULL
        while (i.hasNext) {
          execute(store)(i.next) match {
            case Success(r)     => result = r.asInstanceOf[Num]
            case f @ Failure(_) => return f
          }
        }
        Success(result)
      }
      case Conditional(l, m, r) => {
        execute(store)(l) match {
          case Success(Value.NULL) => execute(store)(r)
          case Success(_)          => execute(store)(m)
          case f @ Failure(_)      => f
        }
      }
      case Loop(l, r) => {
        def doLoop(): Result = {
          while (true) {
            execute(store)(l) match {
              case Success(Value.NULL) => return Success(Value.NULL)
              case Success(v)          => execute(store)(r)
              case f @ Failure(_)      => return f
            }
          }
          Success(Value.NULL)
        }
        Success(Value.NULL)
      }

    }
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c)            => prefix + c.toString
    case UMinus(r)              => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r)             => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r)            => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r)            => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r)              => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r)              => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Variable(v)            => v
    case Assignment(l, r)       => buildExprString(prefix, "Assignment", toFormattedString(prefix)(l), toFormattedString(prefix)(r)) //Totally Functional
    case Conditional(g, l, r)   => buildConditionalString(prefix, "Conditional", g, l, r)
    case Loop(l, r)             => buildLoopString(prefix, "Loop", l, r)
    case Block(statements @ _*) => mapToString(prefix, statements, "Block")
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def mapToString(prefix: String, e: Seq[Expr], nodeString: String): String = {
    val result = new StringBuilder(prefix)
    val strings = e.map(expr => toFormattedString(prefix)(expr))
    strings.foreach(strings => result.append(strings + EOL))
    result.toString
  }

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(INDENT)
    result.append(leftString)
    result.append(",")
    result.append(EOL)
    result.append(INDENT)
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

  def buildAssignmentString(prefix: String, nodeString: String, l: String, r: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(INDENT)
    result.append(l)
    result.append(",")
    result.append(EOL)
    result.append(r)
    result.append(")")
    result.toString
  }

  def buildConditionalString(prefix: String, nodeString: String, g: Expr, l: Expr, r: Expr*) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(INDENT)
    result.append(toFormattedString(prefix)(g))
    result.append(",")
    result.append(EOL)
    result.append(INDENT)
    result.append(toFormattedString(prefix)(l))
    result.append(",")
    result.append(EOL)
    result.append(INDENT)
    r.foreach((block: Expr) => {
      result.append(toFormattedString(prefix)(block))
    })
    result.toString
  }

  def buildLoopString(prefix: String, nodeString: String, l: Expr, r: Expr) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append(prefix)
    result.append("(")
    result.append(EOL)
    result.append(l)
    result.append(prefix)
    result.append(")")
    result.append(r)
    result.toString
  }

  def buildBlockString(prefix: String, expressions: Expr*) = {
    val result = new StringBuilder(prefix)
    result.append("{")
    result.append(EOL)
    result.append(expressions.map(expr => prefix + EOL + INDENT + toFormattedString(prefix)(expr)).mkString(""))
    result.append(prefix)
    result.append("}")
    result.append(EOL)
    result.toString
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
