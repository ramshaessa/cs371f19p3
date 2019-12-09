package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import TestFixtures._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  //println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends FunSuite {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.topLevel, complex1string)
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.topLevel, complex2string)
  val parsedExpr3 = CombinatorParser.parseAll(CombinatorParser.topLevel, assignment1string)
  val parsedExpr4 = CombinatorParser.parseAll(CombinatorParser.topLevel, assignment2string)
  val parsedExpr5 = CombinatorParser.parseAll(CombinatorParser.topLevel, complex3string)
  val parsedExpr6 = CombinatorParser.parseAll(CombinatorParser.topLevel, conditional1string)
  val parsedExpr7 = CombinatorParser.parseAll(CombinatorParser.topLevel, conditional2string)
  val parsedExpr8 = CombinatorParser.parseAll(CombinatorParser.topLevel, block1string)
  val parsedExpr9 = CombinatorParser.parseAll(CombinatorParser.topLevel, loop1string)
  val parsedExpr10 = CombinatorParser.parseAll(CombinatorParser.topLevel, loop2string)

  test("Parse complex1 working") { assert(parsedExpr.get === complex1) }
  test("Parse complex2 working") { assert(parsedExpr2.get === complex2) }
  test("Parse assignment1 working") { assert(parsedExpr3.get === assignment1) }
  test("Parse assignment2 working") { assert(parsedExpr4.get === assignment2) }
  test("Parse complex3 working") { assert(parsedExpr5.get === complex3) }
  test("Parse conditional1 working") { assert(parsedExpr6.get === conditional1) }
  test("Parse conditional2 working") { assert(parsedExpr7.get === conditional2) }
  test("Parse block1 working") { assert(parsedExpr8.get === block1) }
  test("Parse loop1 working") { assert(parsedExpr9.get === loop1) }
  test("Parse loop2 working") { assert(parsedExpr10.get === loop2) }