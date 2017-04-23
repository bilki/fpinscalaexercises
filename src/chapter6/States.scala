package chapter6

import chapter6.Randoms.RNG
import chapter6.Randoms.SimpleRNG

object States {

  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](g: A => State[S, B]): State[S, B] = State(state => {
      val (elem, nextState) = run(state)
      g(elem).run(nextState)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => other.map((b => f(a, b))))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(state => (a, state))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.reverse.foldLeft(unit[S, List[A]](List.empty[A]))((acc, next) =>
        next.map2(acc)((a, l) => a :: l))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ( {}, s))

    def modify[S](f: S => S): State[S, Unit] = {
      val state = get[S]
      state.flatMap(s => set(f(s))).map(s => {})
    }
  }

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def updateMachine(i: Input)(m: Machine): Machine = i match {
    case Coin if (m.candies > 0 && m.locked) => Machine(false, m.candies, m.coins + 1)
    case Turn if (!m.locked)                 => Machine(true, m.candies - 1, m.coins)
    case _                                   => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    states <- State.sequence(inputs.map(i => State.modify[Machine](updateMachine(i) _)))
    s <- State.get[Machine]
  } yield (s.coins, s.candies)
}

object StatesApp extends App {

  import States._

  type Rand[A] = State[RNG, A]

  val simpleRNG = SimpleRNG(42)

  val int = new Rand[Int](_.nextInt)

  println(int.flatMap(random => int.map(_ + random)).run(simpleRNG))

  println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10)))
}
