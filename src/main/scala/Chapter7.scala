import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Chapter7 {
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  sealed trait Future[A] {
    private[Chapter7] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] = _ =>
      new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    def run[A](s: ExecutorService)(a: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)

      a(s) { x => ref.set(x); latch.countDown }
      latch.await
      ref.get
    }

    def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a,b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a,b)))
            }
          }

          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))
        }
      }


    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a,_) => f(a))

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call: Unit = r })

    def lazyUnit[A](a: A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      lazyUnit[B] _ compose f

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))


    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A, B](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars = as map asyncF(a  => if(f(a)) List(a) else List())
      map(sequence(pars))(_.flatten)
    }

    def parFold[A, B](as: IndexedSeq[A])(z: => B)(f: A => B)(m: (B, B) => B): Par[B] = {
      if (as.size <= 1) {
        Par.unit(as.headOption.map(f) getOrElse z)
      } else {
        val (l, r) = as.splitAt(as.length / 2)
        Par.map2(parFold(l)(z)(f)(m), parFold(r)(z)(f)(m))(m)
      }
    }

    def max(ints: IndexedSeq[Int]): Par[Option[Int]] =
      parFold[Int, Option[Int]](ints)(None)(Some(_)){ case (m1, m2) => m1 match {
        case None => m2
        case Some(x) => m2 match {
          case None => m1
          case Some(y) => Some(x max y)
        }
      }
    }

    def words(ps: IndexedSeq[String]): Par[Int] =
      parFold(ps)(0)(_.split("\\s").length)(_ + _)

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      map2(map2(a, b)((_,_)), c){ case ((a, b), c) => f(a,b,c) }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      run(e)(p) == run(e)(p2)

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(n)(choices.apply(_))

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(cond)(c => if (c) t else f)

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      flatMap(key)(choices)

    def flatMap[K, V](key: Par[K])(choices: K => Par[V]): Par[V] =
      es =>
        choices(run(es)(key))(es)

    def join[A](a: Par[Par[A]]): Par[A] =
      es =>
        run(es)(a)(es)

    def flatMap2[K, V](key: Par[K])(choices: K => Par[V]): Par[V] =
      join(map(key)(choices))

    def join2[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(identity[Par[A]])
  }

}
