val l = List("a", "a", "a", "b", "c", "c", "a")
l.span(x=>x=="a")
l.takeWhile(x=>x=="a")
l.dropWhile(x=>x=="a")
pack(l)
encode(l)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

l.foldLeft("")(_+_)
l.foldLeft("")((s: String, s0: String) => s.length  + "+" + s0.length)