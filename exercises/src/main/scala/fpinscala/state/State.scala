package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (intValue, rng2) = int(rng)
    if (intValue < 0 || intValue == Int.MinValue)
      nonNegativeInt(rng2)
    else (intValue, rng2)
  }

  def double(rng: RNG): (Double, RNG) = 
    map(s => nonNegativeInt(s))(_.toDouble / Int.MaxValue.toDouble)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intValue, rng2) = int(rng)
    val (doubleValue, rng3) = double(rng2)
    ((intValue, doubleValue), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doubleValue, rng2) = double(rng)
    val (intValue, rng3) = int(rng2)
    ((doubleValue, intValue), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3) 
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    if (count > 0) {
      val (x, rng2) = int(rng)
      val (xs, rng3) = ints(count-1)(rng2)
      (x :: xs, rng3)
    } else (Nil, rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng)
      (f(a, b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    rng => fs match {
      case (h :: t) => 
        val (x, rng2) = h(rng)
        val (xs, rng3) = sequence(t)(rng2)
        ((x :: xs), rng3)
      case _ =>
        (Nil, rng)
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng2) = f(rng)
      val (b, rng3) = g(a)(rng2)
      (b, rng3)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = 
    flatMap(s => nonNegativeInt(s)) { i => 
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        s => (mod, s)
      else nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = f(a).run(s2)
      (b, s3)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[A, SS >: A](a: A): State[SS, A] = State((s: SS) => (a, s))

  def sequence[A, S](states: List[State[S, A]]): State[S, List[A]] = {
    val acc = State((s: S) => (Nil: List[A], s))
    states.foldRight(acc) { (a: State[S, A], b: State[S, List[A]]) =>
      a.map2(b)(_ :: _)
    }
  }

  type Rand[A] = State[RNG, A]

  // def get[S]: State[S, S] = State(s => (s, s))

  // def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    
    def processInput(input: Input): State[Machine, Unit] =
      State(state => input match {
        case Coin if (state.locked == true) && (state.candies > 0) => 
          ((), state.copy(locked = false, coins = state.coins+1))
        case Turn if (state.locked == false) =>
          ((), state.copy(locked = true, candies = state.candies-1))
        case _ => ((), state)
      })

    null
  }
}
