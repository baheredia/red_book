object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)

    def nonNegativeInt: (Int, RNG) = {
      val (n, nextRNG) = nextInt
      (RNG.pos(n), nextRNG)
    }

    def nextDouble: (Double, RNG) = {
      val (n, nextRNG) = nonNegativeInt
      n.toDouble / Int.MaxValue -> nextRNG
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object RNG {

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      f(a,b) -> rng3
    }

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, nextRng) = f(rng)
      g(a)(nextRng)
    }

    def altMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)( unit[B] _ compose f)

    def altMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => altMap(rb)(b => f(a, b)))

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_,_))

    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
      rs.foldLeft(unit(List.empty[A]))((ns, ra) => map2(ns, ra)((as, a) => a :: as))

    val double: Rand[Double] =
      map(_.nonNegativeInt)(i => i.toDouble / Int.MaxValue)

    val nonNegativeEven: Rand[Int] =
      map(_.nonNegativeInt)(i => i - i % 2)

    val boolean: Rand[Boolean] =
      map(nonNegativeLessThan(2))(i => i == 0)

    def fromOption[A](a: Rand[Option[A]]): Rand[A] = {
      flatMap(a){
        case None => fromOption(a)
        case Some(a) => unit(a)
      }
    }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(_.nonNegativeInt){ i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
      }

    def intBetween(min: Int, maxExclusive: Int): Rand[Int] =
      map(nonNegativeLessThan(maxExclusive - min))(i => i + min)


    def pos(n: Int): Int = {
      if(n >= 0) n
      else n + Int.MinValue
    }

    val intDouble: Rand[(Int, Double)] = both(int, double)

    val doubleInt: Rand[(Double, Int)] = both(double, int)

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = rng.nextDouble
      val (d2, rng2) = rng1.nextDouble
      val (d3, rng3) = rng2.nextDouble
      ((d1, d2, d3), rng3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count <= 0) (List.empty[Int], rng)
      else {
        val (n, nextRng) = rng.nextInt
        val (rest, lastRng) = ints(count-1)(nextRng)
        (n :: rest, lastRng)
      }

    }

    def newInts(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

  }

  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State[S, B](s => {
        val (a, nextS) = run(s)
        f(a).run(nextS)
      })

    def map[B](f: A => B): State[S, B] =
      flatMap( State.unit[S,B] _ compose f)

    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => rb.map(b => f(a, b)))

  }

  object State {
    def unit[S, A](a: A): State[S, A] =
      State((a, _))

    def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
      rs.foldLeft(unit[S, List[A]](List.empty[A]))((ns, ra) => ns.map2(ra)((as, a) => a :: as))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }

}

object CandyMachine {
  import Chapter6.State

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def doNext(input: Input): Machine = input match {
      case Coin => if (locked && (candies > 0)) this.copy(locked = false, coins = coins + 1) else  this
      case Turn => if (locked) this else this.copy(locked = true, candies = candies - 1)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft(State.get[Machine]) {
      case (state, input) =>
        state.map(_.doNext(input))
    }.map(m => (m.coins, m.candies))



}
