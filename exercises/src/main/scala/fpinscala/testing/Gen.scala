package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}


case class Prop(run: (Int, TestCases, RNG) => Result) {

  // todo: create a tag on the failed result to allow tracking
  def &&(p: Prop): Prop = Prop { (maxsize, n,rng) =>
    lazy val r1 = run(maxsize, n, rng)
    lazy val r2 = p.run(maxsize, n,rng)
    if (r1 != Passed) r1 else r2
  }

  // todo: create a tag on the failed result to allow tracking
  def ||(p: Prop): Prop = Prop { (maxsize, n,rng) =>
    lazy val r1 = run(maxsize, n, rng)
    lazy val r2 = run(maxsize, n, rng)
    if (r1 == Passed) r2 else r1
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {

    def isFalsified = true
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

}

case class Gen[A](sample: State[RNG,A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(State { rng =>
      val (a, rng2) = sample.run(rng)
      val (b, rng3) = f(a).sample.run(rng2)
      (b, rng3)
    })

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(rng => RNG.rangedInt(rng)(start, stopExclusive)))

  def boolean: Gen[Boolean] =
    choose(Int.MinValue, Int.MaxValue).map(x => if (x % 2 == 0) false else true)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    weighted((g1, 0.5), (g2, 0.5))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (gen1, p1) = g1
    val (gen2, p2) = g2
    require(p1 + p2 == 1)

    choose(0, Int.MaxValue).flatMap { x =>
      val p1v = p1 * Int.MaxValue
      val p2v = p2 * Int.MaxValue
      if (x < p1v) gen1 else gen2
    }
  }
}

case class SGen[A](g: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(size => g(size).map(f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(size => g(size).flatMap(f))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => Gen.listOfN(size, g))
}
