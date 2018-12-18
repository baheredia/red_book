val a = Chapter5.Stream.cons(1, Chapter5.Stream.empty)


val b = Chapter5.Stream(1,2,3,4)

a.toList
a
b.toList
b
b.drop(2).toList
b.take(2).toList

val c = Chapter5.Stream[Int](1,3,5,2,3,4,5)

c.drop(2).toList

Chapter5.Stream.apply(1,2,3).takeWhile(_ => true)

Chapter5.Stream.map(b)(_ + 1).toList

Chapter5.Stream.take(b, 2).toList

c.zipAll(b).toList
