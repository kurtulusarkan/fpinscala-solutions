package chapter4

object chapter4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift_1[A, B](f: A => B): Option[A] => Option[B] = {
    (a: Option[A]) => a match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def absOption: Option[Double] => Option[Double] = lift(math.abs)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def map2_3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def sequence_3[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => for {
        xx <- x
        yy <- sequence_3(xs)
      } yield xx :: yy
    }
  }

  def insuranceRateQuote(age: Int, numberOfSpeedTickets: Int): Double = {
    age * numberOfSpeedTickets * 2.0
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedTickets: String): Option[Double] = {

    val optAge = Try(age.toInt)
    val optTickets = Try(numberOfSpeedTickets.toInt)

    map2(optAge, optTickets)(insuranceRateQuote)
  }

  //---------------------

  def testFun4(a: Int, b: String, c: Double, d: Int): String = {
    s"$a a, $b b, $c c, $d d"
  }

  def map4[A, B, C, D, E](a: Option[A], b: Option[B], c: Option[C], d: Option[D])(f: (A, B, C, D) => E): Option[E] = {
    for {
      aa <- a
      bb <- b
      cc <- c
      dd <- d
    } yield f(aa, bb, cc, dd)
  }

  def testFun4map4(): Option[String] = {
    map4(Some(4), Some("String"), Some(5.5), Some(3))(testFun4)
  }

  //---------

  def main(args: Array[String]): Unit = {

    println(sequence_3(List(Some(2), Some(3), Some(4))))

    println("Variance: " + variance(List(3, 4, 3, 4, 3, 4, 3, 4, 3)))
    println("absOption: " + absOption(Some(-45.3)))
    println("testFun4map4: " + testFun4map4())
    println("parseInsuranceRateQuote: " + parseInsuranceRateQuote("asdf", "asdf"))
    println("parseInsuranceRateQuote: " + parseInsuranceRateQuote("123", "123"))
  }
}
