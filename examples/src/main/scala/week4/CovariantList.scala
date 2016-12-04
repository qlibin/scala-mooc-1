package week4

trait CovariantList[+T] {
  def head: T
  def tail: CovariantList[T]
  def isEmpty: Boolean
  def prepend[U >: T](elem: U): CovariantList[U] = new Cons(elem, this)
}

object Nil extends CovariantList[Nothing] {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}
class Cons[T](val head: T, val tail: CovariantList[T]) extends CovariantList[T] {
  def isEmpty = false
}

object test {
  val x: CovariantList[String] = Nil
  val y: CovariantList[AnyVal] = new Cons[Int](1, Nil)
}


