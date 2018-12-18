import scala.annotation.tailrec

def fib(n: Int): Int = {
  @tailrec
  def go(n: Int, acc1: Int, acc2: Int): Int = {
    if (n <= 0)
      acc2
    else
      go(n-1, acc1 + acc2, acc1)
  }
  go(n, 1, 0)
}

for (i <- 0 to 10) println(fib(i))


def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @tailrec
  def loop(n: Int, acc: Boolean): Boolean = {
    if (n >= as.length - 1)
      acc
    else if (acc == false) acc
    else loop(n+1, acc && ordered(as(n), as(n+1)))
  }

  loop(0, true)
}

def le(a: Int, b: Int): Boolean = a <= b
def lestr(a: String, b: String): Boolean = a <= b

isSorted(Array(1,2,3,4,5), le)
isSorted(Array(1,2,5,4,5), le)
isSorted(Array(0), le)
isSorted(Array("hello", "Benjamin"), lestr)
isSorted(Array("benjamin", "hellos"), lestr)
isSorted[Int](Array(1,2,3,4,5), (x: Int, y: Int) => x <= y)

def curry[A, B, C](f: (A, B) => C): A => B => C = {
  a => b => f(a, b)
}

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}

