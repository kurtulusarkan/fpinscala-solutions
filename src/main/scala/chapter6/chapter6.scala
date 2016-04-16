package chapter6

object chapter6 {

  def rollDie: Int = {
    val rng = new scala.util.Random
    rng.nextInt(6)
  }


  def main(args: Array[String]) {

    val rng1 = SimpleRNG(42)
    println(s"seed: $rng1")

    val (r1, rng2) = rng1.nextInt
    println(s"seed: $rng2 r1: $r1")

    val (r2, rng3) = rng2.nextInt
    println(s"seed: $rng3 r1: $r2")

    val (r3, rng4) = rng3.nextInt
    println(s"seed: $rng4 r1: $r3")

  }
}
