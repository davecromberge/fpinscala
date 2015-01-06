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

  def map[A,B](f: A => B): Gen[B] = ???

  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    Gen(State(rng => RNG.rangedInt(rng)(start, stopExclusive)))

  def boolean: Gen[Boolean] = 
    Gen(State { rng => 
      val (i, rng2) = RNG.nonNegativeInt(rng)
      if (i % 2 == 0) (false, rng2)
      else (true, rng2)
    })
}

trait SGen[+A] {

}

