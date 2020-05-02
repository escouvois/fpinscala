package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    val f1: Par[C => D] = map2(a, b)((a, b) => (c: C) => f(a, b, c))
    map2(f1, c)((f2: C => D, c: C) => f2(c))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val f1: Par[C => D => E] = map2(a, b)((a, b) => (c: C) => (d: D) => f(a, b, c, d))
    val f2: Par[D => E] = map2(f1, c)((_f1: C => D => E, c: C) => _f1(c))
    map2(f2, d)((_f2: D => E, d: D) => _f2(d))
  }

  def map4WithMap3[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val f1: Par[D => E] = map3(a, b, c)((a, b, c) => (d: D) => f(a, b, c, d))
    map2(f1, d)((_f1, d) => _f1(d))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    val f1 = map4(a, b, c, d)((a, b, c, d) => (e: E) => f(a, b, c, d, e))
    map2(f1, e)((_f1, e) => _f1(e))
  }

  def wordCount(ls: List[String]): Par[Int] = {
    map(
      sequence(
        ls map asyncF(_.split(" ").length)
      )
    )(_.sum)
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((p, acc) => map2(p, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] =
    map {
      sequence {
        ps map asyncF(a => if (f(a)) List(a) else List())
      }
    }(_.flatten)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val i = run(es)(n).get
      choices(i)(es)
    }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(a => if (a) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(key.apply(es).get)(es)

  def chooser[A, B](pa: Par [A])(choices: A => Par[B]): Par[B] =
    es => choices(pa.apply(es).get)(es)

  def flatmap[A, B](pa: Par [A])(choices: A => Par[B]): Par[B] =
    es => choices(pa.apply(es).get)(es)

  def choiceViaFlatmap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatmap(cond)(if (_) t else f)

  def choiceNViaFlatmap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatmap(n)(choices(_))

  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(ppa).get())

  def flatmapViaJoin[A, B](pa: Par [A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  def joinViaFlatmap[A](ppa: Par[Par[A]]): Par[A] =
    flatmap(ppa)(identity)


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

  def main(args: Array[String]): Unit = {
    val es = Executors.newCachedThreadPool()
    val _1to10 = (1 to 10).toList
    val filterOp = parFilter(_1to10)(_ % 2 == 0)
    println("parFilter" + Par.run(es)(filterOp).get)
    val _map3 = map3(Par.unit(_1to10), Par.unit(_1to10), Par.unit(_1to10))((a: List[Int], b: List[Int], c: List[Int]) => a ++ b ++ c)
    println("map3" + Par.run(es)(_map3).get)
    val _wc = wordCount(List("My name is Stain", "I don't complain"))
    println("wordcount " + Par.run(es)(_wc).get)
    val _choice = choice(Par.unit(true))(Par.unit(0), Par.unit(1))
    println("choice " + _choice.apply(es).get)
    val _map = choiceMap(Par.unit("b"))(Map("a" -> Par.unit(0), "b" -> Par.unit(1)))
    println("choiceMap " + _map.apply(es).get)
    val _choiceViaFlatmap = choiceViaFlatmap(Par.unit(true))(Par.unit(0), Par.unit(1))
    println("choiceViaFlatmap " + _choiceViaFlatmap.apply(es).get)
    val _choiceNViaFlatmap = choiceNViaFlatmap(Par.unit(2))(List(Par.unit("a"), Par.unit("b"), Par.unit("c")))
    println("choiceNViaFlatmap " + _choiceNViaFlatmap.apply(es).get)
  }
}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
