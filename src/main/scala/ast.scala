package edu.luc.cs.laufer.cs473.expressions.ast

/** An initial algebra of arithmetic expressions. */
sealed trait Expr
case class Constant(value: Int) extends Expr
abstract class UnaryExpr(expr: Expr) extends Expr { require { expr != null } }
case class UMinus(expr: Expr) extends UnaryExpr(expr)
abstract class BinaryExpr(left: Expr, right: Expr) extends Expr { require { (left != null) && (right != null) } }
abstract class TrinaryExpr(left: Expr, middle: Expr, right: Expr) extends Expr { require { left != null && middle != null && right != null } }
case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Variable(variable: String) extends Expr
case class Assignment(left: Expr, right: Expr) extends Expr
case class Conditional(condition: Expr, left: Expr, right: Expr) extends TrinaryExpr(condition, left, right)
case class Loop(condition: Expr, block: Expr) extends BinaryExpr(condition, block)
case class Block(statements: Expr*) extends Expr
case class Field(ident: String, expr: Expr) extends Expr
case class Struct(fields: List[(String, Expr)]) extends Expr
case class Select(items: List[String]) extends Expr

