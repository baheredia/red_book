import Chapter6.RNG.{intBetween, nonNegativeLessThan}
import Chapter6.{RNG, Rand, State}
import Chapter8.Gen.choose
import Chapter8.Prop
import Chapter8.Prop.{FailedCase, SuccessCount}

object Chapter8 {

  case class Gen[A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(this.sample.flatMap(x => f(x).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap(n => Gen(State.sequence(List.fill(n)(this.sample))))
    }

    def listOf: Gen[List[A]] =
      listOfN(choose(0, Int.MaxValue))

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

    def map[A, B](a: Gen[A])(f: A => B): Gen[B] =
      Gen(a.sample.map(f))

    def map2[A, B, C](a: Gen[A], b: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(a.sample.map2(b.sample)(f))

    def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
      map2(choose(start, stopExclusive), choose(start, stopExclusive))((_, _))

    def toOption[A](a: Gen[A]): Gen[Option[A]] =
      map(a)(Some(_))

    def toOption2[A](a: Gen[A]): Gen[Option[A]] =
      boolean.flatMap(b => if (b) toOption(a) else unit(None))

    def fromOption[A](a: Gen[Option[A]]): Gen[A] =
      Gen(State(RNG.fromOption(a.sample.run)))

    def union[A](g1: Gen[A], g2: Gen[A]):Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val (gen1, w1) = g1
      val (gen2, w2) = g2
      double.flatMap { d =>
        if (d * (w1 + w2) < w1) gen1
        else gen2
      }
    }

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

  case class Prop(run: (TestCases, RNG) => Result) {

    def &&(p: Prop): Prop = Prop { (n, rng) =>
      (run(n, rng), p.run(n, rng)) match {
        case (Passed, Passed) => Passed
        case (Passed, Falsified(f2, n2)) => Falsified(f2, n2 + n)
        case (Falsified(f1, n1), Passed) => Falsified(f1, n1 + n)
        case (Falsified(f1, n1), Falsified(f2, n2)) => Falsified(f1 + f2, n1 + n2)
      }
    }

    def ||(p: Prop): Prop = Prop { (n, rng) =>
      (run(n, rng), p.run(n, rng)) match {
        case (Passed, Passed) => Passed
        case (Passed, Falsified(f2, n2)) => Passed
        case (Falsified(f1, n1), Passed) => Passed
        case (Falsified(f1, n1), Falsified(f2, n2)) => Falsified(f1 + f2, n1 + n2)
      }
    }

  }

  object Prop {

    type FailedCase = String
    type SuccessCount = Int

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) =>
        randomStream(as)(rng).zip(Chapter5.Stream.from(0)).take(n).map {
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

  }

}
