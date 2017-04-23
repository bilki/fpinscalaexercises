package chapter3

import scala.annotation.tailrec

object Trees extends App {

  object Tree {

    sealed trait Tree[+A]

    case class Leaf[A](value: A) extends Tree[A]

    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    // 3.25
    def count[A](t: Tree[A]): Int = {
      @tailrec
      def count0(acc: Int, rest: List[Tree[A]]): Int = rest match {
        case Nil                => acc
        case Leaf(_) :: xs      => count0(acc + 1, xs)
        case Branch(l, r) :: xs => count0(acc + 1, l :: r :: xs)
      }

      count0(0, t :: Nil)
    }

    // 3.26
    def max(t: Tree[Int]): Int = {
      @tailrec
      def max0(acc: Int, rest: List[Tree[Int]]): Int = rest match {
        case Nil                => acc
        case Leaf(v) :: xs      => max0(acc.max(v), xs)
        case Branch(l, r) :: xs => max0(acc, l :: r :: xs)
      }

      max0(Int.MinValue, t :: Nil)
    }

    // 3.27
    def depth[A](t: Tree[A]): Int = {
      @tailrec
      def depth0(acc: Int, rest: List[(Int, Tree[A])]): Int = rest match {
        case Nil                         => acc
        case (depth, Leaf(_)) :: xs      => depth0(acc.max(depth), xs)
        case (depth, Branch(l, r)) :: xs => depth0(acc, (depth + 1, l) :: (depth + 1, r) :: xs)
      }

      depth0(0, (0, t) :: Nil)
    }

    // 3.28
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  import Tree._

  println("nodes t: " + count(Branch(Leaf(2), Branch(Leaf(7), Leaf(9)))))
  println("max t: " + max(Branch(Leaf(2), Branch(Branch(Leaf(7), Leaf(1)), Leaf(0)))))
  println("depth t: " + depth(Branch(Branch(Leaf(2), Branch(Leaf(5), Leaf(3))), Branch(Branch(Leaf(7), Leaf(1)), Leaf(0)))))
  println("depth t2: " + depth(Leaf(2)))
  println("depth t3: " + depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  println("depth t4: " + depth(Branch(Leaf(1), Leaf(2))))
  println("depth t5: " + depth(Branch(Leaf(1), Branch(Branch(Leaf(8), Leaf(9)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))))))
  println("depth t6: " + depth(Branch(Branch(Branch(Branch(Leaf(4), Leaf(3)), Leaf(2)), Leaf(1)), Leaf(0))))
  println(map(Branch(Branch(Branch(Branch(Leaf(4), Leaf(3)), Leaf(2)), Leaf(1)), Leaf(0)))(_ + 1))
}
