

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => xs
  case (z :: zs) :: ys => z :: flatten(zs) ++ flatten(ys)
  case y :: ys => y :: flatten(ys)
}



flatten(List(List(1, 1), 2, List(3, List(5, 8))))