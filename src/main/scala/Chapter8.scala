import Chapter6.RNG.{intBetween, nonNegativeLessThan}
import Chapter6.{RNG, Rand, State}
import Chapter8.Prop.{FailedCase, SuccessCount}

object Chapter8 {

  case class Gen[A](sample: State[RNG, A])

  object Gen {

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(intBetween(start, stopExclusive)))


    def boolean: Gen[Boolean] = {
      Gen(State(RNG.map(nonNegativeLessThan(2))(i => i == 0)))
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      Gen(State.sequence(List.fill(n)(g.sample)))
    }

    def listOf[A](a: Gen[A]): Gen[List[A]] =
      Gen(choose(0, Int.MaxValue).sample.flatMap(n => listOfN(n, a).sample))

    def map[A, B](a: Gen[A])(f: A => B): Gen[B] =
      Gen(a.sample.map(f))

    def map2[A, B, C](a: Gen[A], b: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(a.sample.map2(b.sample)(f))

    def flatMap[A, B](a: Gen[A])(f: A => Gen[B]): Gen[B] =
      Gen(a.sample.flatMap(x => f(x).sample))

    def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
      map2(choose(start, stopExclusive), choose(start, stopExclusive))((_, _))

    def toOption[A](a: Gen[A]): Gen[Option[A]] =
      map(a)(Some(_))

    def toOption2[A](a: Gen[A]): Gen[Option[A]] =
      flatMap(boolean)(b => if (b) toOption(a) else unit(None))

    def fromOption[A](a: Gen[Option[A]]): Gen[A] =
      Gen(State(RNG.fromOption(a.sample.run)))

  }

  trait Prop {

    def check: Either[(FailedCase, SuccessCount), SuccessCount]

    def &&(p: Prop): Prop = new Prop {
      def check: Boolean = this.check && p.check
    }

  }

  object Prop {

    type FailedCase = String
    type SuccessCount = Int

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  }

}
