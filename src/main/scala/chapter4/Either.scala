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

  // if f throws an exception, this does not take care of it.
  def lift[A, B](f: A => B): Either[Exception, A] => Either[Exception, B] =
    _ map f

  // this one takes cares of possible exception that can occurs in f
  def lift_1[A, B](f: A => B): Either[Exception, A] => Either[Exception, B] = {
    case Left(x) => Left(x)
    case Right(x) => Try(f(x))
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case x :: xs => (f(x) map2 traverse(xs)(f))(_ :: _)
    }
  }
}
