package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] =
    foldRight(Nil: List[A])((a, b) => a :: b)

  def take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, b) =>
      if (p(a))
        Stream.cons(a, b)
      else b
    }

  def append[AA >: A](s: Stream[AA]): Stream[AA] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {

    def suffixes: Stream[Stream[A]] =
      unfold(this) {
        case s@Cons(_, t) => Some(s, t())
        case _ => None
      }

    suffixes.map(_.foldRight(z)(f))
  }

  def tails: Stream[Stream[A]] =
    scanRight(Stream.empty[A])((a, b) => Stream.cons(a, b))

  def hasSubsequence[A](s: Stream[A]) =
    tails.exists(_.startsWith(s))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => b.append(f(a)))

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def startsWith[B](s: Stream[B]): Boolean =
    this.zipWith(s)((a,b) => a == b).forAll(_ == true)
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

  val ones: Stream[Int] =
    unfold(0)(counter => Some((1, counter + 1)))

  def constant[A](a: A): Stream[A] =
    unfold(0)(counter => Some((a, counter + 1)))

  def from(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def fibs: Stream[Int] = {
    val seed = (0, 1)
    unfold(seed) {
      case (current, prev) => Some(current+prev, (current+prev, current))
    }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty[A]
    }
}
