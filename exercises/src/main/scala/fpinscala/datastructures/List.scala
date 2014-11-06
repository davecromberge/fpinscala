package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  
  def product(ints: List[Int]): Int = foldLeft(ints, 1)(_ * _)
  
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
    foldLeft(reverse(a1), a2)((xs: List[A], y: A) => Cons(y, xs))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(as, z)((x, y) => f(y, x))
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Cons(_, t) => Cons(h, t)
      case Nil => Nil
    }

  def drop[A](l: List[A], n: Int): List[A] = 
    l match {
      case Cons(_, t) if n > 0 => drop(t, n-1)
      case _ => l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case Nil => Nil
    }

  def length[A](l: List[A]): Int = 
    foldLeft(l, 0)((x, y) => x + 1)
  
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  def toString[A](l: List[A]) = 
    map(l)(_.toString)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def flatten[A](lists: List[List[A]]): List[A] =
    foldLeft(lists, Nil: List[A])(append(_, _))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    l match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

  def hasSubsequence[A](as: List[A], subsequence: List[A]): Boolean = {
    val seed = (subsequence, false)
    val (_, result) = foldRight(as, seed) { (a, state) => 
      state match {
        case (_, true) => state
        case (Nil, false) => (subsequence, true)
        case (Cons(h, t), false) if (a == h) => (t, false)
        case _ => (subsequence, false)
      }
    }
    result
  }
}
