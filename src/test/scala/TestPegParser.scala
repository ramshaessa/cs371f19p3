package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import TestFixtures._

object MainPegParser extends App {
  val parsedExpr = new PegParser(complex1string).InputLine.run()
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestPegParser extends FunSuite {
  val parsedExpr = new PegParser(complex1string).InputLine.run()
  val parsedExpr2 = new PegParser(complex1string2).InputLine.run()
  test("parser works 1") { assert(parsedExpr.get === complex1) }
  test("parser works 2") { assert(parsedExpr2.get === complex1) }
}
