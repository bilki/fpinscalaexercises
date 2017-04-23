package chapter6

import scala.annotation.tailrec

object Randoms {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object SimpleRNG {
    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    // 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
      case (Int.MinValue, nextRng) => (0, nextRng)
      case (ni, nextRng)           => (Math.abs(ni), nextRng)
    }

    // 6.2
    def double(rng: RNG): (Double, RNG) = {
      val (nni, nextRng) = nonNegativeInt(rng)
      (nni.toDouble / Int.MaxValue.toDouble, nextRng)
    }

    // 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (ni, rng2) = nonNegativeInt(rng)
      val (dbl, rng3) = double(rng2)
      ((ni, dbl), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((ni, dbl), rng2) = intDouble(rng)
      ((dbl, ni), rng2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (dbl1, rng2) = double(rng)
      val (dbl2, rng3) = double(rng2)
      val (dbl3, rng4) = double(rng3)
      ((dbl1, dbl2, dbl3), rng4)
    }

    // 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def ints0(acc: List[Int], count0: Int, nextRNG: RNG): (List[Int], RNG) = count0 match {
        case 0 => (acc, nextRNG)
        case n => {
          val (nextInt, rng2) = nextRNG.nextInt
          ints0(nextInt :: acc, n - 1, rng2)
        }
      }

      ints0(Nil, count, rng)
    }

    // 6.5
    def mapDouble(rng: RNG): (Double, RNG) = map(int)(i => i.toDouble)(rng)

    // 6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(int, double)
    val randDoubleInt: Rand[(Double, Int)] = both(double, int)

    // 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.reverse.foldLeft(unit(List.empty[A]))((acc, next) => map2(acc, next)((l, a) => a :: l))

    def seqints(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }

    // 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

    def nnlThanFlatMapped(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    // 6.9
    def fmap[A, B](rnd: Rand[A])(f: A => B): Rand[B] = flatMap(rnd)(r => unit(f(r)))

    def fmap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

}

object RamdomsApp extends App {

  import Randoms._

  println(SimpleRNG.ints(3)(SimpleRNG(42)))
  println(SimpleRNG.seqints(3)(SimpleRNG(42)))

  println(SimpleRNG.nonNegativeLessThan(Int.MaxValue)(SimpleRNG(5)))
  println(SimpleRNG.nnlThanFlatMapped(Int.MaxValue)(SimpleRNG(5)))

  println(map(int)(i => i + 1)(SimpleRNG(85)))
  println(SimpleRNG.fmap(int)(i => i + 1)(SimpleRNG(85)))

  def rollDie: Rand[Int] = map(SimpleRNG.nonNegativeLessThan(6))(_ + 1)

  val zero = rollDie(SimpleRNG(5))._1

  println(zero)
}
