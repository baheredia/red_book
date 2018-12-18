
def mean(xs: Seq[Double]): Chapter4.Option[Double] = {
  if (xs.isEmpty) Chapter4.None
  else Chapter4.Some(xs.sum / xs. length)
}

def variance(xs: Seq[Double]): Chapter4.Option[Double] = {
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

mean(Seq(1,2,3))
mean(Seq())

val a: Chapter4.Option[Int] = Chapter4.Some(1)
val b: Chapter4.Option[Int] = Chapter4.None
val c: Chapter4.Option[Int] = Chapter4.Some(2)

a.map(_  * 2 )
a.getOrElse(4)
a.orElse(Chapter4.Some(2))
Chapter4.None.orElse(Chapter4.Some(2))

Chapter4.Option.sequence(List(a, c, a))

Chapter4.Option.sequence(List(a, c, a, b))
