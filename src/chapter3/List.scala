package chapter3

import scala.annotation.tailrec

sealed trait List[+A] {
  override def toString = {
    "(" + List.foldRight(this, "#")((a, b) => a + "," + b) + ")"
  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](s: List[A], z: B)(f: (A, B) => B): B = {
    s match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  @tailrec
  def foldLeft[A, B](s: List[A], z: B)(f: (B, A) => B): B = {
    s match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(s: List[Int]) =
    foldRight(s, 0)((x, y) => x + y)

  def sum3(s: List[Int]) =
    foldLeft(s, 0)((x, y) => x + y)

  def product(ds: List[Int]): Int = ds match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(s: List[Int]) =
    foldRight(s, 1)((x, y) => x * y)

  def product3(s: List[Int]) =
    foldLeft(s, 1)((x, y) => x * y)

  def length[A](s: List[A]): Int =
    foldRight(s, 0)((_, y) => y + 1)

  def length2[A](s: List[A]): Int =
    foldLeft(s, 0)((x, _) => x + 1)

  def reverse[A](s: List[A]): List[A] =
    foldLeft(s, Nil: List[A])((x, y) => Cons(y, x))

  // efficient right.
  def foldRightViaLeft[A, B](s: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(s), z)((b, a) => f(a, b))

  // non-efficient left
  def foldLeftViaRight[A, B](s: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(s), z)((b, a) => f(a, b))

  def concat[A](ss: List[List[A]]): List[A] = ss match {
    case Nil => Nil
    case Cons(x, xs) => append(x, concat(xs))
  }

  def concat2[A](s: List[List[A]]): List[A] = {
    foldLeft(s, Nil: List[A])(append)
  }

  /*
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  */

  def apply[A](as: A*): List[A] = {

    if (as.isEmpty)
      return Nil

    @tailrec
    def loop(n: Int, acc: List[A]): List[A] = {
      if (n == 0) acc
      else loop(n - 1, Cons(as(n - 1), acc))
    }

    loop(as.size - 1, Cons(as(as.size - 1), Nil))
  }

  def fill[A](n: Int, a: A): List[A] = {
    @tailrec
    def loop(b: Int, acc: List[A]): List[A] = {
      if (b >= n - 1) acc
      else loop(b + 1, Cons(a, acc))
    }
    loop(0, Cons(a, Nil))
  }

  def tail[A](s: List[A]): List[A] = s match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](s: List[A], head: A): List[A] = s match {
    case Nil => Cons(head, Nil)
    case Cons(_, xs) => Cons(head, xs)
  }

  def drop[A](s: List[A], n: Int): List[A] = {
    @tailrec
    def loop(b: Int, acc: List[A]): List[A] = {
      if (n == b) return acc
      acc match {
        case Nil => Nil
        case Cons(x, xs) => loop(b + 1, xs)
      }
    }
    loop(0, s)
  }

  @tailrec
  def drop2[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop2(t, n - 1)
    }

  def dropWhile[A](s: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def loop(b: Int, acc: List[A]): List[A] = {
      acc match {
        case Nil => Nil
        case Cons(x, xs) => if (f(x)) loop(b + 1, xs) else acc
      }
    }
    loop(0, s)
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile2(t, f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  def init[A](s: List[A]): List[A] = s match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def add1(s: List[Int]): List[Int] = {
    s match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, add1(xs))
    }
  }

  def add1_2(s: List[Int]): List[Int] =
    foldRight(s, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doubleToString(s: List[Double]): List[String] = {
    s match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
    }
  }

  def doubleToString2(s: List[Double]): List[String] =
    foldRight(s, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](s: List[A])(f: A => B): List[B] = {
    s match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  def map2[A, B](s: List[A])(f: A => B): List[B] =
    foldRightViaLeft(s, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](s: List[A])(f: A => Boolean): List[A] = {
    s match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) if !f(x) => filter(xs)(f)
    }
  }

  def filter2[A](s: List[A])(f: A => Boolean): List[A] =
    foldRightViaLeft(s, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](s: List[A])(f: A => List[B]): List[B] =
    foldRight(s, Nil: List[B])((a, b) => append(f(a), b))

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter3[A](s: List[A])(f: A => Boolean): List[A] =
    flatMap(s)(x => if (f(x)) List(x) else Nil)

  def zipWithInt(s1: List[Int], s2: List[Int]): List[Int] = (s1, s2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1, b1), Cons(a2, b2)) => Cons(a1 + a2, zipWithInt(b1, b2))
  }

  def zipWith[A, B, C](s1: List[A], s2: List[B])(f: (A, B) => C): List[C] = (s1, s2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1, b1), Cons(a2, b2)) => Cons(f(a1, a2), zipWith(b1, b2)(f))
  }

  @tailrec
  def startsWith[A](sup: List[A], pref: List[A]): Boolean = (sup, pref) match {
    case (_, Nil) => true
    case (Cons(x1, xs1), Cons(x2, xs2)) if x1 == x2 => startsWith(xs1, xs2)
    case _ => false
  }

  @tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, xs) => hasSubSequence(xs, sub)
  }
}
