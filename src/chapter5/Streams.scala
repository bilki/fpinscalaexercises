package chapter5

import scala.annotation.tailrec

object Streams {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

    // 5.1
    def toList: List[A] = {
      @tailrec
      def toList0(acc: List[A], rest: Stream[A]): List[A] = rest match {
        case Empty      => acc
        case Cons(h, t) => toList0(h() :: acc, t())
      }

      toList0(Nil, this).reverse
    }

    // 5.2
    def take(n: Int): Stream[A] = if (n <= 0) Empty else this match {
      case Empty                  => Empty
      case Cons(h, t) if (n == 1) => Stream.cons(h(), Empty)
      case Cons(h, t) if (n > 1)  => Stream.cons(h(), t().take(n - 1))
    }

    def drop(n: Int): Stream[A] = {
      @tailrec
      def drop0(n: Int, rest: Stream[A]): Stream[A] = rest match {
        case Cons(h, t) if (n > 0) => drop0(n - 1, t())
        case _                     => rest
      }

      drop0(n, this)
    }

    // 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
      case _                      => Empty
    }

    // 5.4
    def forAll(p: A => Boolean): Boolean = {
      @tailrec
      def forAll0(rest: Stream[A]): Boolean = rest match {
        case Cons(h, t) => if (p(h())) forAll0(t()) else false
        case Empty      => true
      }

      forAll0(this)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    // 5.5 stream is not evaluated if p(next) is false, so the recursion finishes earlier
    def foldTakeWhile(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((next, stream) => if (p(next)) Stream.cons(next, stream) else Empty)

    // 5.6
    def foldHeadOption: Option[A] =
      foldRight[Option[A]](None)((next, stream) => Some(next))

    // 5.7
    def map[B](f: A => B): Stream[B] =
      foldRight[Stream[B]](Empty)((next, stream) => Stream.cons(f(next), stream))

    def filter(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((next, stream) => if (p(next)) Stream.cons(next, stream) else stream)

    def append[B >: A](other: => Stream[B]): Stream[B] =
      foldRight[Stream[B]](other)((next, stream) => Stream.cons(next, stream))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight[Stream[B]](Empty)((next, stream) => f(next).append(stream))

    // 5.13
    def unMap[B](f: A => B): Stream[B] =
      Stream.unfold(this)(next => next match {
        case Cons(h, t) => Some((f(h()), t()))
        case Empty      => None
      })

    def unTake(n: Int): Stream[A] =
      Stream.unfold((this, n)) {
        case (next, n) => next match {
          case Empty                  => None
          case Cons(h, t) if (n == 1) => Some((h(), (Empty, n - 1)))
          case Cons(h, t) if (n > 1)  => Some((h(), (t(), n - 1)))
        }
      }

    def unTakeWhile(p: A => Boolean): Stream[A] =
      Stream.unfold(this)(next => next match {
        case Cons(h, t) if (p(h())) => Some(h(), t())
        case _                      => None
      })

    def unZipWith[B >: A](other: Stream[B])(f: (A, B) => B): Stream[B] =
      Stream.unfold((this, other)) {
        case (a, b) => (a, b) match {
          case (Empty, Empty)               => None
          case (Empty, Cons(h, t))          => Some((h(), (Empty, t())))
          case (Cons(h, t), Empty)          => Some((h(), (t(), Empty)))
          case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        }
      }

    def unZipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.unfold((this, s2)) {
        case (a, b) => (a, b) match {
          case (Empty, Empty)               => None
          case (Empty, Cons(h, t))          => Some((None, Some(h())), (Empty, t()))
          case (Cons(h, t), Empty)          => Some((Some(h()), None), (t(), Empty))
          case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        }
      }

    // 5.13b
    def hasSubsequence[A](sub: Stream[A]): Boolean = ???

    // 5.14
    def startsWith[A](s: Stream[A]): Boolean =
      unZipAll(s).takeWhile(!_._2.isEmpty).forAll { case (h, h2) => h == h2 }

    // 5.15 (missing final Empty)
    def tails: Stream[Stream[A]] =
      Stream.unfold(this) {
        case s @ Cons(h, t) => Some((s, t()))
        case Empty          => None
      }

    // 5.16
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = ???

    def isEmpty: Boolean
  }
  case object Empty extends Stream[Nothing] {
    override def isEmpty: Boolean = true
  }
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    override def isEmpty: Boolean = false
  }

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // 5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // 5.10
    def fibs: Stream[Int] = {
      def fibs0(v1: Int, v2: Int): Stream[Int] =
        cons(v1, fibs0(v2, v1 + v2))
      fibs0(0, 1)
    }

    // 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((next, state)) => cons(next, unfold(state)(f))
      case None                => Empty
    }

    // 5.12
    def unFibs: Stream[Int] =
      unfold((0, 1))(vs => Some((vs._1, (vs._2, vs._1 + vs._2))))

    def unFrom(n: Int): Stream[Int] =
      unfold(n)(s => Some((s, s + 1)))

    def unConstant[A](a: A): Stream[A] =
      unfold(a)(s => Some((a, a)))
  }
}

object StreamsApp extends App {
  import Streams._

  override def main(args: Array[String]) = {
    lazy val ones: Stream[Int] = Stream.cons(1, ones)

    // 5.12
    lazy val unOnes: Stream[Int] = Stream.unConstant(1)

    val s1 = Stream.from(0)
    val s2 = Stream.from(1000)

    println(unOnes.take(5).toList)

    println(Stream.from(1).unMap(_ * 2).take(5).toList)

    println(Stream.from(20).unTake(5).toList)

    println(Stream.from(30).unTakeWhile(_ < 36).toList)

    println(Stream.fibs.take(7).toList)

    println(Stream.unFibs.take(7).toList)

    println(Stream.unFrom(2).take(5).toList)

    println(Stream.unConstant(3).take(2).toList)

    println(s1.unZipWith(s2)(_ + _).take(5).toList)

    println(s2.unZipAll(s1.take(5)).take(10).toList)

    println(Stream.unFrom(1).take(3).tails.map(s => s.toList).toList)
  }
}
