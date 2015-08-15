package chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def head: Option[A] = this match {
    case Empty => None
    case Cons(x, _) => Some(x())
  }

  def toList: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc:List[A]): List[A] = s match {
      case Cons(x, xs) => loop(xs(), x() :: acc)
      case _ => acc
    }
    loop(this, List()).reverse
  }

  def toList_1: List[A] = {
    this match {
      case Empty => Nil
      case Cons(x, xs) => x() :: xs().toList
    }
  }

  // from original solutions
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
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
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))
}
