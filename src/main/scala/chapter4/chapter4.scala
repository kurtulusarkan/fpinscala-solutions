package chapter4

object chapter4 {

  def meanOption(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def varianceOption(xs: Seq[Double]): Option[Double] = {
    meanOption(xs) flatMap (m => meanOption(xs.map(x => math.pow(x - m, 2))))
  }

  def absOption: Option[Double] => Option[Double] = Option.lift(math.abs)

  // ---------------

  def meanEither(xs: Seq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("Mean of empty list")
    else
      Right(xs.sum / xs.length)
  }

  def varianceEither(xs: Seq[Double]): Either[String, Double] = {
    meanEither(xs) flatMap (m => meanEither(xs.map(x => math.pow(x - m, 2))))
  }

  //-------------------

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    Either.Try(x / y)

  def someFun(a: Int): Int = {
    if (a <= 0) throw new RuntimeException("param is zero")
    a * a
  }

  def safeSomeFun: Either[Exception, Int] => Either[Exception, Int] = Either.lift_1(someFun)

  //-------------------

  def insuranceRateQuote(age: Int, numberOfSpeedTickets: Int): Double = {
    age * numberOfSpeedTickets * 2.0
  }

  def parseInsuranceRateQuoteOption(age: String, numberOfSpeedTickets: String): Option[Double] = {

    val optAge = Option.Try(age.toInt)
    val optTickets = Option.Try(numberOfSpeedTickets.toInt)

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  //---------------------

  def parseInsuranceRateQuoteEither(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Either.Try( age.toInt )
      tickets <- Either.Try { numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)


  def main(args: Array[String]): Unit = {

    println("meanOption: " + meanOption(Vector(3.0, 4.0, 5.0, 6.0, 7.0, 8.0)))
    println("varianceOption: " + varianceOption(List(3, 4, 3, 4, 3, 4, 3, 4, 3)))

    println("meanEither: " + meanEither(Vector(3.0, 4.0, 5.0, 6.0, 7.0, 8.0)))
    println("varianceEither: " + varianceEither(List(3, 4, 3, 4, 3, 4, 3, 4, 3)))

    println("absOption: " + absOption(Some(-45.3)))

    println("safeSomeFun: " + safeSomeFun(Left(new RuntimeException("test"))))
    println("safeSomeFun: " + safeSomeFun(Right(0)))
    println("safeSomeFun: " + safeSomeFun(Right(5)))

    println("parseInsuranceRateQuoteOption: " + parseInsuranceRateQuoteOption("test1", "test2"))
    println("parseInsuranceRateQuoteOption: " + parseInsuranceRateQuoteOption("10", "10"))

    println("parseInsuranceRateQuoteEither: " + parseInsuranceRateQuoteEither("test1", "test2"))
    println("parseInsuranceRateQuoteEither: " + parseInsuranceRateQuoteEither("10", "10"))

  }
}
