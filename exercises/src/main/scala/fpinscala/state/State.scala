package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

// NB - this was called SimpleRNG in the book text
case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    if (i < 0) (-i, rng1) else (i, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i / (Int.MaxValue + 1), rng1)
  }

  val doubleWithMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = int(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = int(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), rng)
    case _ => {
      val (i, r1) = int(rng)
      val (is, r2) = ints(count - 1)(r1)
      (i :: is, r2)
    }
  }

  def nonNegativeInts(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), rng)
    case _ => {
      val (i, r1) = nonNegativeInt(rng)
      val (is, r2) = nonNegativeInts(count - 1)(r1)
      (i :: is, r2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def intsNonNegEven(count: Int): Rand[List[Int]] =
    map(nonNegativeInts(count))(_.map(i => i - i % 2))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a1, rng1) = ra(rng)
    val (a2, rng2) = rb(rng1)
    (f(a1, a2), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val doubleIntMap2: Rand[(Double, Int)] =
    both(double, int)

  val intDoubleMap2: Rand[(Int, Double)] =
    both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(sa)(a => map(sb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def main(args: Array[String]): Unit = {
    val s = Simple(42)
    val (i1, rng1) = int(s)
    println(i1 + " " + rng1)
    val (i2, rng2) = int(rng1)
    println(i2 + " " + rng2)
    val (i3, rng3) = unit(1)(rng1)
    println(i3 + " " + rng3)
    val (i4, rng4) = map(unit(1))(_.toFloat)(rng1)
    println(i4 + " " + rng4)
    println("non-negative int" + nonNegativeInt(s))
    println("ints" + ints(10)(s))
    println("non-negative even" + nonNegativeEven(s))
    println("ints non-negative even" + intsNonNegEven(10)(s))
    println("doubleWithMap" + doubleWithMap(s))
    println("intsSequence" + intsSequence(10)(s))
    println("nonNegativeLessThan" + nonNegativeLessThan(10)(s))
  }
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify[Machine](updateState(i))))
    s <- get
  } yield (s.coins, s.candies)

  def updateState(i: Input) = (m: Machine) => (i, m) match {
    case (_, Machine(_, 0, _))                  => m
    case (Turn, Machine(true, _, _))            => m
    case (Coin, Machine(false, _, _))           => m
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
    case (Coin, Machine(true, candies, coins))  => Machine(false, candies, coins + 1)
  }

  def randomInputs(count: Int): RNG2.Rand[List[Input]] =
    RNG2.intsSequence(10).map(_.map(i => if (i % 2 == 0) Coin else Turn))

  def main(args: Array[String]): Unit = {
    val inputs = randomInputs(10).run(Simple(42))._1
    println("Inputs :" + inputs)
    println(simulateMachine(inputs).run(Machine(false, 5, 10))._1)
  }
}

object RNG2 {

  type Rand[A] = State[RNG, A]

  def intsSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(State(_.nextInt)))

  def main(args: Array[String]): Unit = {
    println("intsSequence" + intsSequence(10).run(Simple(42)))
  }

}

