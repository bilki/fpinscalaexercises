package chapter4

import scala.annotation.tailrec

object Options {

  sealed trait Option[+A] {
    // 4.1
    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Some(f(v))
      case None    => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case None    => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = flatMap(v => if (f(v)) Some(v) else None)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  // 4.2
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.size > 0)
      Some(xs.sum / xs.size)
    else
      None
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(elem => math.pow(elem - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  import chapter2.Currifying._

  // 4.3
  def map2lifted[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    lift(curry(f))(a).flatMap(subf => lift(subf)(b))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(v1 => b.map(v2 => f(v1, v2)))

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    a.flatMap(v1 => b.flatMap(v2 => c.map(v3 => f(v1, v2, v3))))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.reverse.foldLeft[Option[List[A]]](Some(Nil))((prev, next) => map2(next, prev)(_ :: _))

  def recSeq[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil       => Some(Nil)
    case next :: l => map2(next, recSeq(l))(_ :: _)
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def traverse0(optList: Option[List[B]], rest: List[A]): Option[List[B]] = rest match {
      case Nil     => optList
      case x :: xs => traverse0(map2(f(x), optList)(_ :: _), xs)
    }

    traverse0(Some(Nil), a.reverse)
  }

  def trasequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}

object OptionsApp extends App {

  import Options._

  val intList = List(0, 1, 2, 3)

  val optList = List(Some(1), Some(-7), Some(3))

  def toIntOpt(i: Int): Option[Int] = if (i >= 0) Some(i * 2) else None

  println(traverse(intList)(toIntOpt))

  println(trasequence(optList))
}
