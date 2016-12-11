import math.Ordering


def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, y :: ys1) => ys
        case (x :: xs1, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

msort(List(4,6,45,2,7,8,65,4,3))
msort(List("orange", "apple", "pineapple", "banana"))