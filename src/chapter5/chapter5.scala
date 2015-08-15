package chapter5

object chapter5 {

  def main(args: Array[String]) {

    println("Stream: " + Stream(1,2,3,4,5))
    println("toList: " + Stream(1,2,3,4,5).toList)
    println("drop: " + Stream(1,2,3,4,5).drop(2).toList)
    println("take: " + Stream(1,2,3,4,5).take(2).toList)
    println("takeWhile: " + Stream(1,2,3,4,5).takeWhile(_ < 3).toList)
  }
}
