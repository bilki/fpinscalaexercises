package chapter7

import java.time.LocalDateTime
import java.util.concurrent._

object Parallels {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def run[A](s: ExecutorService)(c: Par[A]): Future[A] = c(s)

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val pares = run(es)(pa)
      val pbres = run(es)(pb)
      UnitFuture(f(pares.get, pbres.get))
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a, _) => f(a))
    }

    def fork[A](a: => Par[A]): Par[A] = es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

    def delay[A](a: => Par[A]): Par[A] = es => a(es)

    // 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.reverse.foldLeft(unit(List.empty[A]))((comps, nextComp) => map2(nextComp, comps)((comp, l) => comp :: l))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    // 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val valueExists = parMap(as)(a => if (f(a)) Nil else List(a))
      map(valueExists)(_.flatten)
    }
  }

}

object ParallelsApp extends App {

  import Parallels.Par._

  val threadPool = Executors.newFixedThreadPool(16)

  def waitAndSum(x: Int): Int = {
    Thread.sleep(2000)
    x + 1
  }

  val futureTwo = asyncF(waitAndSum)(1)
  val futureThree = asyncF(waitAndSum)(2)

  val nums = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val futureLessThanSix = parFilter(nums) { n => Thread.sleep(1000); n < 6 }
  println(LocalDateTime.now())
  println(run(threadPool)(futureLessThanSix).get)
  println(LocalDateTime.now())

  println(LocalDateTime.now())
  println(run(threadPool)(sequence(List(futureTwo, futureThree, futureTwo))).get)
  println(LocalDateTime.now())

  threadPool.shutdown
}