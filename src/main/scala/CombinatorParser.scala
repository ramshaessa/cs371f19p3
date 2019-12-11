package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast.{Expr, _}

object CombinatorParser extends JavaTokenParsers {

  def topLevel: Parser[Expr] =
    rep1(statement) ^^ {
      case s => Block(s: _*)
    }

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! rep(("+" | "-") ~ term) ^^ {
      case l ~ r =>
        r.foldLeft(l) {
          case (a, "+" ~ t) => Plus(a, t)
          case (a, "-" ~ t) => Minus(a, t)
        }
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ r =>
        r.foldLeft(l) {
          case (a, "*" ~ t) => Times(a, t)
          case (a, "/" ~ t) => Div(a, t)
          case (a, "%" ~ t) => Mod(a, t)
        }
    }
  /*
    def struct: Parser[Expr] =
      "{" ~ "}" ^^ { case "{" ~ "}" => Struct() } |
        "{" ~ rep1sep(field, ",") ~ "}" ^^ {
          case _ ~ fs => Struct(fs: _*)
        }
  */
  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    rep1sep(ident, ".") ^^ { case reference => Select(reference) }
    | wholeNumber ^^ { case s => Constant(s.toInt) }
    | ident ^^ { case i => Variable(i) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ { case s => Variable(s.toString) }
    | struct
  )

  def field: Parser[(String, Expr)] =
    ident ~ ":" ~ expr ^^ { case i ~ ":" ~ e => (i, e) }

  def struct: Parser[Expr] =
    "{" ~ "}" ^^ { case "{" ~ "}" => Struct(List[(String, Expr)]()) } |
      "{" ~ rep1sep(field, ",") ~ "}" ^^ {
        case _ ~ fs ~ _ => Struct(fs)
      }

  /** statement   ::= expression ";" | assignment | conditional | loop | block */
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ { case e ~ _ => e }
    | assignment
    | conditional
    | loop
    | block
  )

  /** assignment  ::= ident "=" expression ";" */
  // Assignment(Variable(...)..)
  // Assignment(Select(   string*),..)
  def assignment: Parser[Expr] =
    ident ~ "=" ~ expr ~ ";" ^^ {
      case i ~ _ ~ e ~ _ => Assignment(Variable(i), e)
    }

  // /** conditional ::= "if" "(" expression ")" block [ "else" block ] */
  //conditional ::= "if" "(" expression ")" block [ "else" block ]
  def conditional: Parser[Expr] =
    "if" ~ "(" ~ expr ~ ")" ~ block ~ opt("else" ~ block) ^^ {
      case "if" ~ _ ~ e ~ _ ~ l ~ None             => Conditional(e, l, Block())
      case "if" ~ _ ~ e ~ _ ~ l ~ Some("else" ~ r) => Conditional(e, l, r)
    }

  /** loop        ::= "while" "(" expression ")" block */
  def loop: Parser[Expr] =
    "while" ~ "(" ~ expr ~ ")" ~ block ^^ {
      case "while" ~ _ ~ e ~ _ ~ b => Loop(e, b)
    }

  /** block       ::= "{" statement* "}" */
  def block: Parser[Expr] =
    "{" ~ rep(statement) ~ "}" ^^ {
      case _ ~ s ~ _ => Block(s: _*)
    }
}
