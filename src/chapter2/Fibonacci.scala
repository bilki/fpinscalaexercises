package chapter2

import scala.annotation.tailrec

object Fibonacci extends App {
  override def main(args: Array[String]) = {

    def fib(n: Int): Int = {
      @tailrec
      def fib0(num: Int, sum: Int, prev: Int): Int = {
        if (num > n) sum
        else if (num == 0 || num == 1) fib0(num + 1, sum + num, sum)
        else fib0(num + 1, sum + prev, sum)
      }

      fib0(0, 0, 0)
    }

    println("0: " + fib(0))
    println("1: " + fib(1))
    println("2: " + fib(2))
    println("3: " + fib(3))
    println("4: " + fib(4))
    println("5: " + fib(5))
    println("6: " + fib(6))
    println("7: " + fib(7))
    println("8: " + fib(8))
    println("9: " + fib(9))
  }
}
