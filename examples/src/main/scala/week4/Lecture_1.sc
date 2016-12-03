
// Peano numbers

object Zero extends Nat {
  override def isZero: Boolean = true
  override def predecessor: Nat = throw new Error("0.predessesor")
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat =
    if (that.isZero) this else throw new Error("negative")
  override def toString: String = "0"
}


Zero - Zero
val n1 = new Succ(Zero)
val n2 = new Succ(Zero).successor
val n3 = new Succ(Zero).successor.successor
val n4 = new Succ(Zero).successor.successor.successor
val n7 = n4 + n2 + n1
val n6 = n7 - n1
n7 - n3
n4 - n4


class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def predecessor: Nat = n
  override def +(that: Nat): Nat =
    if (that.isZero) this
    else new Succ(n + that)
  override def -(that: Nat): Nat =
    if (that.isZero) this
    else n - that.predecessor
  override def toString: String = {
    def counter(n:Nat, count: Int): Int =
      if (n.isZero) count else counter(n.predecessor, count + 1)
    counter(n, 1).toString
  }
}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

