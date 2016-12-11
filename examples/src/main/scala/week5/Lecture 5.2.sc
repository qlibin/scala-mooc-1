def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, y :: ys1) => ys
    case (x :: xs1, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }

merge(List(3, 4,5, 7), List(2,4,6, 8))

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

msort(List(4,6,45,2,7,8,65,4,3))