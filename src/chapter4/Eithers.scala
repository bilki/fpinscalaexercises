package chapter4

import scala.annotation.tailrec

object Eithers {

  sealed trait Either[+E, +A] {
    // 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e)  => Left(e)
      case Right(v) => Right(f(v))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e)  => Left(e)
      case Right(v) => f(v)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e)  => b
      case Right(v) => Right(v)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield {
        f(aa, bb)
      }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  // 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @tailrec
    def traverse0(eitherList: Either[E, List[B]], rest: List[A]): Either[E, List[B]] = rest match {
      case Nil     => eitherList
      case x :: xs => traverse0(f(x).map2(eitherList)(_ :: _), xs)
    }

    traverse0(Right(Nil), as.reverse)
  }

  // 4.8
  // Change the mkName to return a list of errors
}

object EithersApp extends App {

  import Eithers._

  val eitherList = List(1, 5, -2)

  def intToEither(i: Int): Either[String, Int] =
    if (i == 5)
      Left("This is five")
    else if (i >= 0)
      Right(i + 2)
    else
      Left("Less than zero")

  println(traverse(eitherList)(intToEither))

  val s = (1 to 500000).toStream

  println("25k: " + s.take(200000))
}
