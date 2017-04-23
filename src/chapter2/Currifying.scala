package chapter2

object Currifying extends App {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def sum(a: Int, b: Int): Int = a + b

  println("curryfied sum: " + curry(sum)(2)(3))

  println("uncurryfied sum: " + uncurry(curry(sum))(2, 3))
}