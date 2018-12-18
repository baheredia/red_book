object Chapter4 {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this.map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this.map(Some(_)).getOrElse(ob)


    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {

    def some[A](a: A): Option[A] = Some(a)

    def none[A]: Option[A] = None

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)


    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case x::xs => f(x).flatMap(b => traverse(xs)(f).map(bs => b::bs))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
  }

  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(value) => Left(value)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(value) => Left(value)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(value) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)

  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {

    def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = a match {
      case Nil => Right(Nil)
      case x::xs =>
        for {
          b <- f(x)
          bs <- traverse(xs)(f)
        } yield b::bs
    }

    def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = traverse(a)(x => x)

  }

}
