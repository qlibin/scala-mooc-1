package week2

// Using symbolyc method names

object lecture_2_7_operators {
  val x = new Rational(1, 2)
  x.numer
  x.denom

  val y = new Rational(2, 3)

  -(x + y - x)
  x + y - -x

  y + y

  val a = new Rational(1, 3)
  val b = new Rational(5, 7)
  val c = new Rational(3, 2)

  a - b - c

  a + b * c

  x < y

  y<x

  x max y
  y max x

  a/c

  assert(y.max(x).numer == 2, "!")
}

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must not be zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- : Rational = new Rational(-x, y)

  def - (that: Rational) = this + -that

  def * (that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def / (that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)

  def < (than: Rational) = numer * than.denom < than.numer * denom

  def max(that: Rational) = if (this < that) that else this

  override def toString = s"$numer/$denom"
}

