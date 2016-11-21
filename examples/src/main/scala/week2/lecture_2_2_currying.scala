package week2

object lecture_2_2_currying {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  def fact(n: Int) = product(x => x)(1, n)

  fact(4)

  def general(stopValue: Int, f: Int => Int, operation: (Int, Int) => Int)(a: Int, b: Int): Int =
    if (a > b) stopValue
    else operation(f(a), product(f)(a + 1, b))

  general(0, x => x, (x, y) => x + y)(1, 4)

}