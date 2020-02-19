package chapter2

object Fibonacci {

  def fib(b: Int): Int = {

    @scala.annotation.tailrec
    def loop(b: Int, current: Int, prev: Int): Int = {
      if (b == 1) prev
      else loop(b - 1, current + prev, current)
    }
    loop(b, 1, 0)
  }

  // }
}
