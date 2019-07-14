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
      case Branch(l, r) => 1 + depth(l) max depth(r)
    }

  def map[A, B](t: Tree[A])(implicit f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l), map(r))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l), fold(r))
    }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maxFold(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depthFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _ max _)

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
}