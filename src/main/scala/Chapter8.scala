import Chapter6.RNG.{intBetween, nonNegativeLessThan}
import Chapter6.{RNG, Rand, State}
import Chapter8.Gen.choose
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

  trait Prop {

    def check: Either[(FailedCase, SuccessCount), SuccessCount]

    def &&(p: Prop): Prop = new Prop {
      def check: Either[(FailedCase, SuccessCount), SuccessCount] = (this.check, p.check) match {
        case (Left((f1, nSuc1)), Left((f2, nSuc2))) => Left((f1 + f2, nSuc1 + nSuc2))
        case (Left((f, nSuc1)), Right(succ)) => Left((f, nSuc1 + succ))
        case (Right(nSuc1), Left((f, nSuc2))) => Left((f, nSuc1 + nSuc2))
        case (Right(nSuc1), Right(nSuc2)) => Right(nSuc1 + nSuc2)
      }
    }

  }

  object Prop {

    type FailedCase = String
    type SuccessCount = Int

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  }

}
