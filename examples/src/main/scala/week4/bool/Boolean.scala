package week4.bool

// Lecture 1
abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T
  def && (x: => Boolean): Boolean = ifThenElse(x, false_)
  def || (x: => Boolean): Boolean = ifThenElse(true_, x)
  def unary_!: Boolean = ifThenElse(false_, true_)
  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)
  def < (x: Boolean): Boolean = ifThenElse(false_, x)
  def > (x: Boolean): Boolean = ifThenElse(x.unary_!, false_)
  def <= (x: Boolean): Boolean = ifThenElse(x, true_)
  def >= (x: Boolean): Boolean = ifThenElse(true_, x)
}

object true_ extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = t
}
object false_ extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = e
}
