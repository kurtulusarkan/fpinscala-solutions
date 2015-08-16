package chapter5

object chapter5 {

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

  def main(args: Array[String]) {

    val x = maybeTwice(true, {
      println("hi")
      1 + 41
    })

    val x2 = maybeTwice2(true, {
      println("hi")
      1 + 41
    })

    println("maybeTwice: " + x)
    println("maybeTwice2: " + x2)

    println("if2: " + if2(cond = false, sys.error("whoa"), 3))
    println()

    val stream = Stream(1, 2, 3, 4, 5)

    println("Stream: " + stream)
    println()
    println("head: " + stream.head)
    println()
    println("head_1: " + stream.head_1)
    println()
    println("tail: " + stream.tail)
    println()
    println("tail: " + Stream.empty)
    println()

    println("toList: " + stream.toList)
    println()

    println("drop: " + Stream(1, 2, 3, 4, 5).drop(2).toList)
    println()
    println("dropWhile: " + Stream(1, 2, 3, 4, 5).dropWhile(_ < 3).toList)
    println()

    println("take: " + Stream(1, 2, 3, 4, 5).take(2).toList)
    println()
    println("takeWhile: " + Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList)
    println()
    println("takeWhile_1: " + Stream(1, 2, 3, 4, 5).takeWhile_1(_ < 3).toList)
    println()

    println("exists: " + Stream(1, 2, 3, 4, 5).exists(_ == 20))
    println()
    println("exists_1: " + Stream(1, 2, 3, 4, 5).exists_1(_ == 2))
    println()

    println("forAll: " + Stream(1, 2, 3, 4, 5).forAll(_ > 0))
    println()
    println("forAll: " + Stream(1, 2, 3, 4, 5).forAll(_ > 2))
    println()

    println("map.filter: " + Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList)
    println()

    println("ones_1: " + Stream.ones_1.map(_ + 1).exists(_ % 2 == 0))

    println("constant_2: " + Stream.constant_2(5).take(5).toList)

    println("constant_2: " + Stream.from_2(5).take(5).toList)

    println("fibs_2: " + Stream.fibs_2().takeWhile(_ < Int.MaxValue / 2).toList)
    println()
  }
}
