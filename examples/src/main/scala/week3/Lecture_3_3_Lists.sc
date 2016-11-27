
val list = newList(1, newList(2, newList(3, newList(4, nil))))
list.get(3)
list.get(0)
list.get(2)

def newList[T](head: T, tail: List[T]): Cons[T] = new Cons[T](head, tail)
def nil[T] = new Nil[T]

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def get(index: Int): T
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  override def get(index: Int): T =
    if (index == 0) head
    else if (index > 0) tail.get(index - 1)
    else throw new IndexOutOfBoundsException()

}
class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")

  override def get(index: Int): T = throw new IndexOutOfBoundsException()
}