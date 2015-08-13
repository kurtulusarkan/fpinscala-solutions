package chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(x) => Left(x)
      case Right(x) => Right(f(x))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(x) => Left(x)
      case Right(x) => f(x)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(x) => Right(x)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      x <- this
      y <- b
    } yield f(x, y)
  }

  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this flatMap (x => b map (f(x, _)))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

}
