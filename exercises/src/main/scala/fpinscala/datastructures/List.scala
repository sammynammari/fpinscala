package fpinscala.datastructures

import scala.annotation.tailrec

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
    case Nil => sys.error("list is Nil")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) { case (_, i) => i + 1 }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumLeft(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def productLeft(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)

  def lengthLeft[A](xs: List[A]): Int = foldLeft(xs, 0) { (i, _) => i + 1 }

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A]) { (t, h) => Cons(h, t) }

  def appendRight[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys) { Cons(_, _) }

  def concatenate[A](xss: List[List[A]]): List[A] = foldRight(xss, Nil: List[A])(append)

  def addOne(xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int]){ (h, t) => Cons(h + 1, t) }

  def toString(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String]){ (h, t) => Cons(h.toString, t) }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B]) { (h, t) => Cons(f(h), t) }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A]) { (a, as) => if (f(a)) Cons(a, as) else as }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concatenate(map(l)(f))

  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def elementwiseAdd(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xt), Cons(y, yt)) => Cons(x + y, elementwiseAdd(xt, yt))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil)                   => Nil
    case (Nil, _)                   => Nil
    case (Cons(a, at), Cons(b, bt)) => Cons(f(a, b), zipWith(at, bt)(f))
  }

  def all(xs: List[Boolean]): Boolean = foldRight(xs, true)(_ && _)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val isPrefix = all(zipWith(sup, sub)(_ == _)) && length(sub) <= length(sup)

    sup match {
      case Cons(_, _) if (isPrefix) => true
      case Cons(_, ps)              => hasSubsequence(ps, sub)
      case Nil                      => false
    }
  }
}
