package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("set head of empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => sys.error("drop an empty list")
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => sys.error("drop an empty list")
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /* To prevent stackoverflow */
  def foldRightWithFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A, B](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  def concat[A](as : List[List[A]]): List[A] =
    foldRight(as, Nil:List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

  def double2String(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightWithFoldLeft(l, Nil:List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightWithFoldLeft(l, Nil:List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
  }

  def main(args: Array[String]): Unit = {
    val l1 = List(1,2,3,4)
    val l2 = List(1.0,2.0,3.0,4.0)
    println("length: " + length(l1))
    println("init:" + init(l1))
    println("reverse:" + reverse(l1))
    println("appendFoldRight:" + appendFoldRight(l1, l1))
    println("appendFoldLeft:" + appendFoldLeft(l1, l1))
    println("add 1: " + add1(l1))
    println("double2String: " + double2String(l2))
    println("map: " + map(l1)(_ + 1))
    println("filter: " + filter(l1)(_ < 3))
    println("flatMap: " + flatMap(l1)(i => List(i, i)))
    println("filterWithFlatMap: " + filterWithFlatMap(l1)(_ < 3))
    println("addPairwise: " + addPairwise(l1, l1))
  }
}
