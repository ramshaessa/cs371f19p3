class TestExecute {

  import edu.luc.cs.laufer.cs473.expressions._
  import TestFixtures._
  import edu.luc.cs.laufer.cs473.expressions.behaviors.executor.Num
  import org.scalatest.FunSuite

  import scala.util.Success

  class TestExecute extends FunSuite {
    test("Execute complex1 working") {
      val result = behaviors.executor.execute(complex1)
      assert(result === Success(Num(-1)))
    }

    test("Execute complex2 working") {
      val result = behaviors.executor.execute(complex2)
      assert(result === Success(Num(-4)))
    }

    test("Execute assignment1 working") {
      val result = behaviors.executor.execute(assignment1)
      assert(result === Success(Num(0)))
      assert(behaviors.executor.instance("x") === Num(5))
    }

    test("Execute assignment2 working") {
      val result = behaviors.executor.execute(assignment2)
      assert(result === Success(Num(0)))
      assert(behaviors.executor.instance("y") === Num(7))
      assert(behaviors.executor.instance("x") === Num(5))
    }

    test("Execute complex3 working") {
      val result = behaviors.executor.execute(complex3)
      val exception = intercept[java.lang.NoSuchFieldException] {
        result.get
      }
      assert(exception.getMessage === "y2")
    }

    test("Execute conditional1 working") {
      val result = behaviors.executor.execute(conditional1)
      assert(result === Success(Num(0)))
      assert(behaviors.executor.instance("x") === Num(2))
    }

    test("Execute conditional2 working") {
      val result = behaviors.executor.execute(conditional2)
      assert(result === Success(Num(0)))
      assert(behaviors.executor.instance("x") === Num(2))
    }

    test("Execute block1 working") {
      val result = behaviors.executor.execute(block1)
      val exception = intercept[java.lang.NoSuchFieldException] {
        result.get
      }
      assert(exception.getMessage === "r")
    }

    test("Execute loop1 working") {
      val result = behaviors.executor.execute(loop1)
      assert(result === Success(Num(0)))
    }

    test("Execute loop2 working") {
      val result = behaviors.executor.execute(loop2)
      assert(result === Success(Num(0)))
      assert(behaviors.executor.instance("y") === Num(3))
      assert(behaviors.executor.instance("x") === Num(2))
      assert(behaviors.executor.instance("r") === Num(0))
    }
  }

}
