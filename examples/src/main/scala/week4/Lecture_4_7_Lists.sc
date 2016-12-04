
//val l = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
val l = List(9,8,7,6,5,4,3,2,1,0)
//val l = List(9,8,7,6,5,4,3,2,1,0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10)

var count = 0
isort(l)
println("count: " + count)


def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => {
    println(x); count = count + 1
    if (x <= y) x :: xs else y :: insert(x, ys)
  }
}

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

