package chapter2

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FibonacciSpec extends AnyFreeSpec with Matchers {

  "Fib" - {
    "should return 0 when 1 is inputted to fib" in {
      Fibonacci.fib(1) shouldBe 0
    }
    "should return 1 when 2 is inputted into fib" in {
      Fibonacci.fib(2) shouldBe 1
    }

    "should return 1 when 3 is inputted into fib" in {
      Fibonacci.fib(3) shouldBe 1
    }

    "should return 13 when 8 is inputted into fib" in {
      Fibonacci.fib(8) shouldBe 13
    }

    "should return 144 when 13 is inputted into fib" in {
      Fibonacci.fib(13) shouldBe 144
    }
  }

}
