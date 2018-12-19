import Chapter6.RNG.{intBetween, nonNegativeLessThan}
import Chapter6.{RNG, Rand, SimpleRNG, State}
import Chapter8.Gen.{boolean, choose}
import Chapter8.Prop.{FailedCase, SuccessCount}

object Chapter8 {

  case class Gen[+A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(this.sample.flatMap(x => f(x).sample))

    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f))

    def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(sample.map2(b.sample)(f))

    def toOption[A]: Gen[Option[A]] =
      map(Some(_))

    def union[AA >: A](g2: Gen[AA]):Gen[AA] =
      boolean.flatMap(b => if (b) this else g2)


    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap(n => Gen(State.sequence(List.fill(n)(this.sample))))
    }

    def listOf: SGen[List[A]] =
      SGen(n => listOfN(Gen.unit(n)))

    def listOf1: SGen[List[A]] =
      SGen(n => listOfN(Gen.unit(n+1)))

    def unsized: SGen[A] = SGen(_ => this)

  }

  object Gen {

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(intBetween(start, stopExclusive)))

    val double: Gen[Double] = {
      Gen(State(RNG.double))
    }

    val boolean: Gen[Boolean] = {
      Gen(State(RNG.map(nonNegativeLessThan(2))(i => i == 0)))
    }

    def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
      choose(start, stopExclusive).map2(choose(start, stopExclusive))((_, _))

    def toOption2[A](a: Gen[A]): Gen[Option[A]] =
      boolean.flatMap(b => if (b) a.toOption else unit(None))

    def fromOption[A](a: Gen[Option[A]]): Gen[A] =
      Gen(State(RNG.fromOption(a.sample.run)))

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val (gen1, w1) = g1
      val (gen2, w2) = g2
      double.flatMap { d =>
        if (d * (w1 + w2) < w1) gen1
        else gen2
      }
    }
  }

  case class SGen[+A](forSize: Int => Gen[A]) {

    def flatMap[B](f: A => SGen[B]): SGen[B] =
     SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

    def map[B](f: A => B): SGen[B] =
      SGen(forSize andThen (_.map(f)))

    def map2[B, C](b: SGen[B])(f: (A, B) => C): SGen[C] =
      SGen(n => forSize(n).map2(b.forSize(n))(f))

    def union[AA >: A](b: SGen[AA]): SGen[AA] =
      SGen[AA](n => forSize(n).union(b.forSize(n)))

  }

  object SGen {
    def unit[A](a: A): SGen[A] = Gen.unit(a).unsized
  }

  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  type MaxSize = Int

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

    def &&(p: Prop): Prop = Prop { (m, n, rng) =>
      run(m, n, rng) match {
        case Passed => p.run(m, n, rng)
        case Falsified(f, n1) => Falsified(f, n1)
      }
    }

    def ||(p: Prop): Prop = Prop { (m, n, rng) =>
      run(m, n, rng) match {
        case Passed => Passed
        case Falsified(_, _) => p.run(m, n, rng)
      }
    }

  }

  object Prop {

    type FailedCase = String
    type SuccessCount = Int

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g.forSize(_))(f)

    def forAll[A](as: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (m, n, rng) =>
        val casesPerSize = (n + (m - 1)) / m
        val props: Chapter5.Stream[Prop] =
          Chapter5.Stream.from(0).take((n min m) + 1).map {
            i => forAll(as(i))(f)
          }
        val prop: Prop = props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
        prop.run(m, n, rng)
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (m, n, rng) => randomStream(as)(rng).zip(Chapter5.Stream.from(0)).take(n).map{
        case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }.find(_.isFalsified).getOrElse(Passed)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Chapter5.Stream[A] =
      Chapter5.Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests: \n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
      }

  }

  object Test {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(smallInt.listOf1) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    val sortedProp = Prop.forAll(smallInt.listOf1) { ns =>
      val sorted = ns.sorted
      sorted.zip(sorted.tail).forall { case (a: Int, b: Int) => a <= b }
    }
  }

}
