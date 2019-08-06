package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def toListTailRec: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty      => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, Nil).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _          => true
  }

  def forAllFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty      => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this) {
      case Cons(h, t) if n > 1  => Some(h(), t())
      case Cons(h, t) if n == 1 => Some(h(), empty)
      case Empty                => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    }

  def zipWithViaUnfold[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = ???
  /*unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }*/

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) =>
        Some((None[A], Some(h2())), (None[A], t2()))
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] =
      cons(first, go(second, first + second))
    go(0, 1)
  }

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (x, y) => Some(x, (y, x + y)) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some(1, 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => empty
  }

  def main(args: Array[String]): Unit = {
    val s1 = Stream(1, 2, 3, 4)
    val s2 = Stream(1.0, 2.0, 3.0, 4.0)
    println("toList: " + s1.toList)
    println("toListTailRec: " + s1.toListTailRec)
    println("exists: " + s1.exists(_ > 3))
    println("take: " + s1.take(3).toList)
    println("drop: " + s1.drop(3).toList)
    println("takeWhile: " + s1.takeWhile(_ < 3).toList)
    println("takeWhileFoldRight: " + s1.takeWhileFoldRight(_ < 3).toList)
    println("forAll: " + s1.forAll(_ < 6))
    println("forAll: " + s1.forAll(_ < 3))
    println("forAllFoldRight: " + s1.forAll(_ < 6))
    println("forAllFoldRight: " + s1.forAll(_ < 3))
    println("headOption: " + s1.headOption)
    println("map: " + s1.map(_ + 10).toList)
    println("filter: " + s1.filter(x => 0 == x % 2).toList)
    println("append: " + s1.append(s1.map(_ + 10)).toList)
    println("flatMap: " + s1.flatMap(x => Stream(x, x * x)).toList)
    println(
      "ones.map(_ + 1).exists(_ % 2 == 0): " + ones
        .map(_ + 1)
        .exists(_ % 2 == 0)
    )
    println("ones.takeWhile(_ == 1): " + ones.takeWhile(_ == 1))
    println("ones.forAll(_ != 1): " + ones.forAll(_ != 1))
    println("constant: " + constant(1))
    println("from: " + from(1).take(40).toList)
    println("fibs: " + fibs.take(40).toList)
    println("unfold: " + unfold(10) { x =>
      if (x == 0) None else Some(x, x - 1)
    }.take(50).toList)
    println("fibsViaUnfold: " + fibsViaUnfold.take(30).toList)
    println("fromViaUnfold: " + fromViaUnfold(-10).take(30).toList)
    println("constantViaUnfold: " + constantViaUnfold(-10).take(30).toList)
    println("onesViaUnfold: " + onesViaUnfold.take(30).toList)
    println("mapViaUnfold: " + s1.mapViaUnfold(_ + 10).toList)
    println("takeViaUnfold: " + s1.takeViaUnfold(2).toList)
    println("takeWhileViaUnfold: " + s1.takeWhileViaUnfold(_ < 3).toList)
    println(
      "zipWithViaUnfold: " + s1.zipWithViaUnfold(s1.map(1 + _))(_ * _).toList
    )
  }
}
