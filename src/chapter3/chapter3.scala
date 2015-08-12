package chapter3

object chapter3 {

  def main(args: Array[String]): Unit = {

    val res = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(res)

    val l = List(1, 2, 3, 4, 5)
    println("Original: " + l)
    println("Sum: " + List.sum(l))
    println("Sum2: " + List.sum2(l))
    println("Sum3: " + List.sum3(l))
    println("Product: " + List.product(l))
    println("Product2: " + List.product2(l))
    println("Product3: " + List.product3(l))
    println("Tail: " + List.tail(l))
    println("setHead 10: " + List.setHead(l, 10))
    println("Drop 2: " + List.drop(l, 2))
    println("DropWhile 1: " + List.dropWhile(l)(_ == 1))
    println("DropWhile 2: " + List.dropWhile(l)(_ == 2))
    println("Fill 5,5: " + List.fill(5, 5))
    println("Append 1,2 + 4,5: " + List.append(List(1, 2), List(4, 5)))
    println("Init: " + List.init(l))
    println("Length: " + List.length(l))
    println("Length2: " + List.length2(l))
    println("Add1: " + List.add1(l))
    println("Add1_2: " + List.add1_2(l))
    println("doubleToString: " + List.doubleToString(List(1.0, 2.0, 3.0)))
    println("Map double to String: " + List.map(List(1.0, 2.0, 3.0))(_.toString))
    println("Reverse: " + List.reverse(l))
    println("Concat: " + List.concat(List(List(1, 2), List(3, 4))))
    println("Concat2: " + List.concat(List(List(1, 2), List(3, 4))))
    println("Filter: " + List.filter(l)(_ < 3))
    println("Filter2: " + List.filter2(l)(_ < 3))
    println("Filter3: " + List.filter3(l)(_ < 3))
    println("FlatMap: " + List.flatMap(l)(x => List(x, x)))
    println("FlatMap2: " + List.flatMap2(l)(x => List(x, x)))
    println("hasSubSequence: " + List.hasSubSequence(List(1, 2, 3, 4, 5, 6), List(2, 3, 4)))
    println("hasSubSequence: " + List.hasSubSequence(List(1, 2, 3, 4, 5, 6), List(4, 3)))
    println(List.foldRight(l, Nil: List[Int])(Cons(_, _)))

    val tree: Tree[String] = new Branch(new Branch(new Leaf("a"), new Leaf("b")), new Branch(new Leaf("c"), new Leaf("d")))
    println("Tree: " + tree)
    println("Tree.size: " + Tree.size(tree))
    println("Tree.maximum: " + Tree.maximum[String](tree, (a, b) => if (a > b) a else b))
    println("Tree.depth: " + Tree.depth(tree))
    println("Tree.map: " + Tree.map(tree)(_.toUpperCase))
    println("Tree.size2: " + Tree.size2(tree))
    println("Tree.maximum2: " + Tree.maximum2[String](tree, (a, b) => if (a > b) a else b))
    println("Tree.depth2: " + Tree.depth2(tree))
    println("Tree.map2: " + Tree.map2(tree)(_.toUpperCase))
  }
}
