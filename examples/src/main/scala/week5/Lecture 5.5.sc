

mapFun(List(1,2,3,4,5), (x:Int)=>x*2)
lengthFun(List(1,2,3,4,5))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) :: _ )
def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (t: T, i: Int) => i+1 )


def sum0(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
def product0(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)

def sum1(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
def product1(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

def sum2(xs: List[Int]) = (xs foldLeft 0) (_ + _)
def product2(xs: List[Int]) = (xs foldLeft 1) (_ * _)