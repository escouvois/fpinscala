package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
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

  val doubleWithMap: Rand[Int] =
    map(nonNegativeInt)(_ / (Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = int(rng)
    val (d, r2) = double(r1)
    ((i,d),r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = int(r1)
    ((d,i),r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), rng)
    case _ => {
      val (i, r1) = int(rng)
      val (is, r2) = ints(count-1)(r1)
      (i::is, r2)
    }
  }

  def nonNegativeInts(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), rng)
    case _ => {
      val (i, r1) = nonNegativeInt(rng)
      val (is, r2) = nonNegativeInts(count-1)(r1)
      (i::is, r2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def intsNonNegEven(count: Int): Rand[List[Int]] =
    map(nonNegativeInts(count))(_.map(i => i - i % 2))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a1, rng1) = ra(rng)
    val (a2, rng2) = rb(rng1)
    (f(a1, a2), rng2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val doubleIntMap2: Rand[(Double, Int)] =
    both(double, int)

  val intDoubleMap2: Rand[(Int, Double)] =
    both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  def main(args: Array[String]): Unit = {
    val s = Simple(42)
    val (i1, rng1) = int(s)
    println(i1+ " "+ rng1)
    val (i2, rng2) = int(rng1)
    println(i2+ " "+ rng2)
    val (i3, rng3) = unit(1)(rng1)
    println(i3+ " "+ rng3)
    val (i4, rng4) = map(unit(1))(_.toFloat)(rng1)
    println(i4+ " "+ rng4)
    println("non-negative int"+ nonNegativeInt(s))
    println("ints"+ ints(10)(s))
    println("non-negative even" + nonNegativeEven(s))
    println("ints non-negative even" + intsNonNegEven(10)(s))
    println("doubleWithMap" + doubleWithMap(s))
    println("intsSequence" + intsSequence(10)(s))
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
