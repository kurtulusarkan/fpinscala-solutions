package chapter2

import scala.annotation.tailrec

object chapter2 {

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, next: Int): Int = {
      if (n <= 0) prev
      else go(n - 1, next, prev + next)
    }
    go(n, 0, 1)
  }

  def formatResult(name: String, x: Int, f: Int => Int) = {
    "The %s of %d is %d".format(name, x, f(x))
  }

  def findFirst[A](ss: Array[A])(p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n > ss.length - 1) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[A](ss: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= ss.length - 1) true
      else if (!ordered(ss(n), ss(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def abs(n: Int): Int = if (n < 0) -n else n

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def unCurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {

    println(formatResult("Abs", -42, abs))
    println(formatResult("Factorial", 5, factorial))
    println(formatResult("Fibonacci", 20, fibonacci))

    val arr = Array[String]("aaa", "bbb", "ccc")

    println("FindFirst: " + findFirst(arr)(x => x == "ccc"))
    println("isSorted: " + isSorted(arr)((a, b) => a < b))

    def sum(a: Int, b: Int): Int = a + b

    val partialSum = partial1(5, sum)
    val curriedSum = curry(sum)
    val unCurrySum = unCurry(curriedSum)
    val composed = compose((x: Int) => x + x, (y: Int) => y * y)

    println("partialSum: " + partialSum(5))
    println("curriedSum: " + curriedSum(5)(5))
    println("unCurrySum: " + unCurrySum(5, 5))
    println("composed: " + composed(5))
  }
}
