package chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def head: Option[A] = this match {
    case Empty => None
    case Cons(x, _) => Some(x())
  }

  def tail: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(x, y) => Some(y())
  }

  def toList: List[A] = {

    // for head, tail traces
    println("----- toList")

    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(x, xs) => loop(xs(), x() :: acc)
      case _ => acc
    }

    loop(this, List()).reverse
  }

  def toList_1: List[A] = {
    this match {
      case Empty => Nil
      case Cons(x, xs) => x() :: xs().toList_1
    }
  }

  // from original solutions
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, xs) if n > 0 => xs().drop(n - 1)
      case _ => this
    }
  }

  @tailrec
  final def dropWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(x, xs) if p(x()) => xs().dropWhile(p)
      case _ => this
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(x, xs) if n > 1 => Stream.cons(x(), xs().take(n - 1))
      case Cons(x, _) if n == 1 => Stream.cons(x(), Stream.empty)
      case _ => Stream.empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(x, xs) if p(x()) => Stream.cons(x(), xs().takeWhile(p))
      case _ => Stream.empty
    }
  }

  @tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(x, xs) => p(x()) || xs().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(x, xs) => f(x(), xs().foldRight(z)(f))
      case _ => z
    }
  }

  def exists_1(p: A => Boolean): Boolean =
    foldRight(false)((x, y) => p(x) || y)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, y) => p(x) && y)

  def takeWhile_1(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)
  }

  def head_1: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => {
      println("_map:" + a); Stream.cons(f(a), b)
    })

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => {
      println("_filter:" + a); if (f(a)) Stream.cons(a, b) else b
    })

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] =
    filter(p).head
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {

    // modified see evaluation of head's and tail's
    lazy val head = {
      lazy val h = hd; println("_head:" + h); h
    }
    lazy val tail = {
      lazy val t = tl; println("_tail:" + t); t
    }

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply_1[A](as: A*): Stream[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply_1(as.tail: _*))

  def apply[A](as: A*): Stream[A] = {

    if (as.isEmpty)
      return empty

    @tailrec
    def loop(n: Int, acc: Stream[A]): Stream[A] = {
      if (n == 0) acc
      else loop(n - 1, cons(as(n - 1), acc))
    }

    loop(as.length - 1, cons(as(as.length - 1), empty))
  }

  def ones: Stream[Int] = Stream.cons(1, ones)

  def constant_1[A](a: A): Stream[A] = {
    cons(a, constant_1(a))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = {
      cons(a, loop(b, a + b))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def ones_1: Stream[Int] =
    unfold(1)(s => Some((1, 1)))

  def constant_2[A](a: A): Stream[A] =
    unfold(a)(Some(_,a))

  def from_2(n: Int): Stream[Int] =
    unfold(n)(a => Some(a, a+1))

  def fibs_2(): Stream[Int] =
    unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }
}
