package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(x) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth(t: Tree[Int]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maxFold(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depthFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _ max _)

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    val t =
      Branch(
        Branch(
          Branch(Leaf(1), Leaf(2)),
          Branch(Leaf(13), Leaf(4))),
        Branch(
          Branch(Leaf(5),
            Branch(Leaf(15), Leaf(9))
          ),
          Branch(Leaf(12), Leaf(8))
        )
      )
    println(s"size: ${size(t)}")
    println(s"maximum: ${maximum(t)}")
    println(s"depth: ${depth(t)}")
    println(s"map: ${map(t)(1 + _)}")
  }
}