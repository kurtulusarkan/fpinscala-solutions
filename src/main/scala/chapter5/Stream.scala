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
      println("_map:" + a)
      Stream.cons(f(a), b)
    })

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => {
      println("_filter:" + a)
      if (f(a)) Stream.cons(a, b) else b
    })

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] =
    filter(p).head

  def map_1[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(x, xs) => Some(f(x()), xs())
      case _ => None
    }

  def take_1(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(x, xs), 1) => Some(x(), (Stream.empty, 0))
      case (Cons(x, xs), y) if y > 1 => Some(x(), (xs(), y - 1))
      case _ => None
    }

  def takeWhile_2(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(x, xs) if f(x()) => Some(x(), xs())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some(f(x1(), x2()), (xs1(), xs2()))
      case _ => None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  // from books solutions.
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) ->(t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  @tailrec
  final def startsWith[B >: A](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Cons(x1, xs1), Cons(x2, xs2)) if x1() == x2() => xs1().startsWith(xs2())
    case _ => false
  }

  // from book's solutions
  final def startsWith_1[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  // from book's solutions
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def hasSubSequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  // from book's solutions
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {

    // modified to see evaluation of head's and tail's
    lazy val head = {
      lazy val h = hd
      println("_head:" + h)
      h
    }
    lazy val tail = {
      lazy val t = tl
      println("_tail:" + t)
      t
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def ones_1: Stream[Int] =
    unfold(1)(s => Some((1, 1)))

  def constant_2[A](a: A): Stream[A] =
    unfold(a)(Some(_, a))

  def from_2(n: Int): Stream[Int] =
    unfold(n)(a => Some(a, a + 1))

  def fibs_2(): Stream[Int] =
    unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }
}
