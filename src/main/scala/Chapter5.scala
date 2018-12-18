object Chapter5 {

  sealed trait Stream[+A] {

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n <= 0) Empty
        else Stream.cons[A](h(),t().take(n-1))
    }

    def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(_, t) =>
        if (n <= 0) this
        else t().drop(n-1)
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

    def headOption: Option[A] =
      foldRight[Option[A]](None)((a, b) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((a, bs) => Stream.cons(f(a), bs))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((a, as) => if (p(a)) Stream.cons(a, as) else as)

    def append[AA >: A](other: => Stream[AA]): Stream[AA] =
      foldRight(other)((a, as) => Stream.cons(a, as))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((a, bs) => f(a).append(bs))

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

    def zipWith[B,C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, bs)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]):Stream[(Option[A], Option[B])] =
      Stream.unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Stream.empty[B]))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Stream.empty[A], t2()))
        case _ => None
      }

    def startsWith[AA >: A](sub: Stream[AA]): Boolean =
      this.zipWith(sub)((a,b) => (a,b)).forAll{ case (a,b) => a == b }

    def tails: Stream[Stream[A]] =
      Stream.unfold((this, false)){ case (as, isLast) => as match {
        case Cons(h, t) => Some((as, (t(), isLast)))
        case Empty  if isLast => None
        case _ => Some(Stream.empty, (Stream.empty, true))
      }}

    def hasSubsequence[AA >: A](sub: Stream[AA]): Boolean =
      tails.exists(_.startsWith(sub))

    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
      foldRight(Stream(z)){ case (a, bs) =>
          Stream.cons(f(a, bs.headOption.getOrElse(z)), bs)
      }


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
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = {
      lazy val st: Stream[A] = cons(a, st)
      st
    }

    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def fibsFrom(a: Int, b: Int): Stream[Int] =
        cons(a, fibsFrom(b, a+b))

      fibsFrom(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => Stream.empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

    def fibs2: Stream[Int] =
      Stream.unfold[Int, (Int, Int)]((0, 1))(p => Some((p._1, (p._2, p._1 + p._2))))

    def from2(n: Int): Stream[Int] =
      Stream.unfold(n)(m => Some((m, m+1)))

    def constant2[A](a: A): Stream[A] =
      Stream.unfold(a)(s => Some((a, s)))

    def ones: Stream[Int] = Stream.constant(1)


    def map[A, B](a: Stream[A])(f: A => B): Stream[B] =
      unfold(a){
        case Empty => None
        case Cons(h, t) => Some((f(h()), t()))
      }

    def take[A](a: Stream[A], n: Int): Stream[A] =
      unfold[A, (Stream[A], Int)]((a, n)){ case (as, m) => as match {
        case Empty => None
        case Cons(h, t) => if (m <= 0) None else Some((h(), (t(), m-1)))
      }}

    def takeWhile[A](a: Stream[A], p: A => Boolean): Stream[A] =
      unfold(a){
        case Cons(h, t) if p(h()) => Some(h() -> t())
        case _ => None
      }

  }

}
