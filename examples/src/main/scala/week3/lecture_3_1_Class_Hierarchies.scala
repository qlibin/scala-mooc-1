object lecture_3_1_Class_Hierarchies {
  def main(args: Array[String]): Unit = {
    val t1 = new NonEmpty(3, Empty, Empty)
    val t2 = t1.incl(7).incl(1).incl(5).incl(2).incl(3)
    val t3 = new NonEmpty(4, Empty, Empty).incl(6).incl(9)
    println(t3)
  }
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((right union left) union other) incl elem
//    right.union(left).union(other).incl(elem)
//    right.union(left.union(other).incl(elem))

  override def toString = "{" + left + elem + right + "}"

}