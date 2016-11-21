class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must not be zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational = new Rational(-x, y)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def div(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)

  def less(than: Rational) = numer * than.denom < than.numer * denom

  def max(that: Rational) = if (this.less(that)) that else this

  override def toString = s"$numer/$denom"
}

val x = new Rational(1, 2)
x.numer
x.denom

val y = new Rational(2, 3)

x.add(y).sub(x).neg

y.add(y)

val a = new Rational(1, 3)
val b = new Rational(5, 7)
val c = new Rational(3, 2)

a.sub(b).sub(c)

a.add(b).mul(c)

x.less(y)

y.less(x)

x.max(y)
y.max(x)

assert(y.max(x).numer == 2, "!")