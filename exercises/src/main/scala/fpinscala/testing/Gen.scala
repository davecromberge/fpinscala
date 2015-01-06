package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop { self =>
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check = self.check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG,A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(State { rng =>
      val (a, rng2) = sample.run(rng)
      val (b, rng3) = f(a).sample.run(rng2)
      (b, rng3)
    })
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

trait SGen[+A] {

}

