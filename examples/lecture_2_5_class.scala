class Rational(x: Int, y: Int) {

  def numer = x

  def denom = y

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)
  
  def neg: Rational = new Rational(-x, y)
  
  def sub(that: Rational) = add(that.neg)
  
  def mul(that: Rational) =
    new Rational(numer * that.numer,
      denom * that.denom)
  
  def div(that: Rational) =
    new Rational(numer * that.denom,
      denom * that.numer)
  
  override def toString = s"$numer/$denom"
}

val x = new Rational(1, 2)
x.numer
x.denom

val y = new Rational(2, 3)

x.add(y).sub(x).neg

val a = new Rational(1, 3)
val b = new Rational(5, 7)
val c = new Rational(3, 2)

a.sub(b).sub(c)

a.add(b).mul(c)
