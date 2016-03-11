package chapter7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

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

    //    def asyncF[A,B](f: A => B): A => Par[B] = es =>
    //      fork(lazyUnit(f))

    def run[A](s: ExecutorService)(c: Par[A]): Future[A] = c(s)

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val pares = run(es)(pa)
      val pbres = run(es)(pb)
      UnitFuture(f(pares.get, pbres.get))
    }

    def fork[A](a: => Par[A]): Par[A] = es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })
  }
}

object ParallelsApp extends App {
  import Parallels._

  override def main(args: Array[String]) = {
  }
}