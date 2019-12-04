package edu.luc.cs.laufer.cs473.expressions.ast

import java.beans.Statement

/** An initial algebra of arithmetic expressions. */
sealed trait Expr

case class Constant(value: Int) extends Expr
abstract class UnaryExpr(expr: Expr) extends Expr { require { expr != null } }
case class UMinus(expr: Expr) extends UnaryExpr(expr)
abstract class BinaryExpr(left: Expr, right: Expr) extends Expr { require { (left != null) && (right != null) } }
case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Variable(name: String) extends Expr {
  require(name != null)

}
case class Block(statement: Expr*) extends Expr
case class Cond(guard: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr
case class Loop(left: Expr, right: Expr) extends Expr
case class Assign(left: Variable, right: Expr) extends Expr
