package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._
import org.parboiled2._

object CombinatorParser extends JavaTokenParsers {

  def topLevel: Parser[Expr] =
    rep(statement) ^^ {
      case s => Block(s: _*)
    }

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! opt(("+" | "-") ~ term) ^^ {
      case l ~ None          => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None          => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
  )

  def assign: Parser[Expr] =
    ident ~ "=" ~ expr ~ ";" ^^ {
      case i ~ _ ~ e ~ _ => Assign(Variable(i), e)

    }

  def statement: Parser[Expr] =
    expr ~ ";" ^^ { case e ~ _ => e } | assign | loop | cond | block

  def block: Parser[Expr] =
    "{" ~ rep(statement) ~ "}" ^^ {
      case _ ~ statements ~ _ =>
        Block(statements: _*)

    }

  def cond: Parser[Expr] =
    "if" ~ "(" ~ expr ~ ")" ~ block ~ opt("else" ~ block) ^^ {
      case "if" ~ _ ~ expr ~ _ ~ l ~ None             => Cond(expr, l, Block())
      case "if" ~ _ ~ expr ~ _ ~ l ~ Some("else" ~ r) => Cond(expr, l, r)

    }

  def loop: Parser[Expr] =
    "while" ~ "(" ~ expr ~ ")" ~ block ^^ {
      case "while" ~ _ ~ e ~ _ ~ b => Loop(e, b)
    }

}
