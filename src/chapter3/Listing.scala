package chapter3

import scala.annotation.tailrec

object List {
  sealed trait List[+A] {
    def tail: List[A]
    def setHead[T >: A](newHead: T): List[T]
    def drop(n: Int): List[A]
    def dropWhile(f: A => Boolean): List[A]
    def init: List[A]
    def length: Int
  }

  case object Nil extends List[Nothing] {
    override def tail: List[Nothing] = Nil
    override def setHead[Nothing](newHead: Nothing): List[Nothing] = Nil
    override def drop(n: Int): List[Nothing] = Nil
    override def dropWhile(f: Nothing => Boolean): List[Nothing] = Nil
    override def init: List[Nothing] = Nil
    override def length: Int = 0
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def setHead[T >: A](newHead: T): List[T] = Cons(newHead, tail)

    override def drop(n: Int): List[A] = {
      @tailrec
      def drop0(rest: List[A], n: Int): List[A] = {
        if (n == 0) rest
        else drop0(rest.tail, n - 1)
      }

      drop0(this, n - 1)
    }

    override def dropWhile(f: A => Boolean): List[A] = {
      @tailrec
      def drop0(rest: List[A]): List[A] = {
        rest match {
          case Nil         => Nil
          case Cons(x, xs) => if (!f(x)) rest else drop0(xs)
        }
      }

      drop0(this)
    }

    override def init: List[A] = {
      // TODO @tailrec
      def drop0(acc: List[A], rest: List[A]): List[A] = {
        rest match {
          case Nil          => Nil
          case Cons(x, Nil) => acc
          case Cons(x, xs)  => Cons(x, drop0(acc, xs))
        }
      }

      drop0(Nil, this)
    }

    // 3.9
    override def length: Int = {
      @tailrec
      def length0(acc: Int, rest: List[A]): Int = rest match {
        case Nil         => acc
        case Cons(x, xs) => length0(acc + 1, xs)
      }

      length0(0, this)
    }

  }

  ////////////// 
  // STATIC METHODS
  //////////////

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Maybe we can stop the recursion if we know the inverse of z, then we match against that in
  // the pattern matching and stop the recursion
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def foldLeft0(acc: B, as: List[A]): B = as match {
      case Nil         => acc
      case Cons(x, xs) => foldLeft0(f(acc, x), xs)
    }

    foldLeft0(z, as)
  }

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // 3.11
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((total, _) => total + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((total, elem) => Cons(elem, total))

  // 3.13
  def tailRecFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((total, elem) => f(elem, total))
  // foldLeft can be implemented with foldRight, but the problem is that it will never be tail rec, e.g. init?
  // Or maybe it is because associativity of List type

  // 3.14
  def append[A](elem: A, l: List[A]): List[A] = foldLeft(reverse(l), Nil: List[A])((total, next) => total match {
    case Nil => Cons(next, Cons(elem, total))
    case _   => Cons(next, total)
  })

  // 3.15 O(2n + m) where n is the total number of elements and m is the number of sublists
  def flatten[A](ll: List[List[A]]): List[A] = foldLeft(reverse(ll), Nil: List[A]) { (total, nextList) =>
    @tailrec
    def insertAllNextList(acc: List[A], rest: List[A]): List[A] = rest match {
      case Nil         => acc
      case Cons(x, xs) => insertAllNextList(Cons(x, acc), xs)
    }

    insertAllNextList(total, reverse(nextList))
  }

  // 3.16
  def sumOne(l: List[Int]): List[Int] = foldLeft(reverse(l), Nil: List[Int])((total, next) => Cons(next + 1, total))

  // 3.17
  def doubleToString(l: List[Double]): List[String] = foldLeft(reverse(l), Nil: List[String])((total, next) => Cons(next.toString, total))

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldLeft(reverse(as), Nil: List[B])((total, next) => Cons(f(next), total))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def filter0(acc: List[A], rest: List[A]): List[A] = rest match {
      case Nil         => acc
      case Cons(x, xs) => if (f(x)) filter0(Cons(x, acc), xs) else filter0(acc, xs)
    }

    filter0(Nil, reverse(as))
  }
  
  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  // 3.21
  def filterFlat[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(elem => if (f(elem)) Cons(elem, Nil) else Nil)

  // 3.22 & 3.23 TODO fix order errors
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def zipWith0(acc: List[A], l1rest: List[A], l2rest: List[A]): List[A] = (l1rest, l2rest) match {
      case (Nil, Nil)                   => acc
      case (Nil, Cons(e2, ys))          => zipWith0(Cons(e2, acc), Nil, ys)
      case (Cons(e1, xs), Nil)          => zipWith0(Cons(e1, acc), xs, Nil)
      case (Cons(e1, xs), Cons(e2, ys)) => zipWith0(Cons(f(e1, e2), acc), xs, ys)
    }

    zipWith0(Nil, reverse(l1), reverse(l2))
  }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def hasSub0(supRest: List[A], subRest: List[A]): Boolean = (supRest, subRest) match {
      case (Nil, Nil)                 => true
      case (Cons(x, xs), Nil)         => true
      case (Nil, Cons(y, ys))         => false
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) hasSub0(xs, ys) else hasSub0(xs, sub)
    }

    hasSub0(sup, sub)
  }
}

object Listing extends App {
  import List._

  override def main(args: Array[String]) = {

    // 3.8 We can use foldRight to implement apply, it is equivalent
    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

    val l = List("a", "b", "c", "d")

    println("l sum: " + foldLeft(l, "")(_ + _))

    println("l length: " + length(l))

    println("l reverse: " + reverse(l))

    println("l sum: " + tailRecFoldRight(l, "")(_ + _))

    println("l appended: " + append("e", l))

    println("l flatten: " + flatten(List(List(1, 2), List(2, 3, 1), List(5, 6))))

    println("l sum one: " + sumOne(List(1, 2, 3)))

    println("l double to string: " + doubleToString(List(1.2, 2.2, 3.5)))

    println("l test map power: " + map(List(1, 2, 3))(n => n * n))

    println("l filter odds: " + filter(List(1, 2, 3, 4))(n => n % 2 == 0))

    println("l flatmap: " + flatMap(List(1, 2, 3))(i => List(i, i)))

    println("l filter flat: " + filter(List(1, 2, 3, 4))(n => n % 2 == 0))

    println("l1 zipwith l2: " + zipWith(List(1, 2, 3), List(4, 5, 6, 2))((a, b) => a + b))

    println("l has sub: " + hasSubsequence(List(2, 4, 8, 6, 9), List(4, 8)))
  }
}
