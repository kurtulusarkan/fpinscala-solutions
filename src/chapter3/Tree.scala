package chapter3

sealed trait Tree[+A] {
  override def toString = {
     Tree.fold(this)(_.toString)((x,y) => "["+x+","+y+"]")
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(x) => x
    case Branch(l, r) => f(maximum(l, f), maximum(r, f))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(x) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](s: Tree[A])(f: A => B): Tree[B] = s match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](s: Tree[A])(f: A => B)(g: (B, B) => B): B = s match {
    case Leaf(x) => f(x)
    case Branch(x, y) => g(fold(x)(f)(g), fold(y)(f)(g))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximum2[A](t: Tree[A], f: (A, A) => A): A =
    fold(t)(a => a)((x, y) => f(x, y))

  def depth2[A](t: Tree[A]): Int =
    fold(t)(a => 0)((x, y) => 1 + (x max y))

  def map2[A, B](s: Tree[A])(f: A => B): Tree[B] =
    fold(s)(a => Leaf(f(a)):Tree[B])(Branch(_,_))
}
