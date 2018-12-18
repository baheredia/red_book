package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](xs: List[A], x: A): List[A] = xs match {
    case Nil => Nil
    case Cons(y, ys) => Cons(x, ys)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (n <= 0) l
      else drop(xs, n-1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (!f(x)) l
      else dropWhile(xs, f)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_:A, n: Int) => n + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((n, _) => n+1)

  def append[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)(Cons(_, _))

  def join[A](lls: List[List[A]]): List[A] =
    foldLeft(lls, Nil[A])(append)

  def add1(ints: List[Int]): List[Int] =
    foldRight(ints, Nil[Int])((n, ls) => Cons(n+1, ls))

  def insideString(ds: List[Double]): List[String] =
    foldRight(ds, Nil[String])((d, ls) => Cons(d.toString, ls))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil[B])((a, bs) => Cons(f(a), bs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil[A])((a, bs) => if (f(a)) Cons(a, bs) else bs)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    join(map(as)(f))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil[A]) else Nil[A])

  def addLists(as: List[Int], bs: List[Int]): List[Int] =
    as match {
      case Nil => Nil
      case Cons(x, xs) =>
        bs match {
          case Nil => Nil
          case Cons(y, ys) => Cons(x + y, addLists(xs, ys))
        }
    }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) =>
        bs match {
          case Nil => Nil
          case Cons(y, ys) => Cons(f(x,y), zipWith(xs, ys)(f))
        }
    }
  }

  def take[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) Nil
    else as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x, take(xs, n-1))
    }
  }

  def isEmpty[A](as: List[A]): Boolean = as match {
    case Nil => true
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil =>
      if (isEmpty(sub)) true
      else false
    case Cons(x, xs) =>
      val subb = take(sup, length(sub))
      if (subb == sub) true
      else hasSubsequence(xs, sub)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = {
    case Leaf(a) => a
    case Branch(l: Tree[Int], r: Tree[Int]) => max(l) max max(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A], z: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(a) => z(a)
    case Branch(left, right) => f(fold(left, z)(f), fold(right, z)(f))
  }

}
