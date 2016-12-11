

class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff+"x^"+exp) mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(2 -> 4.0, 3 -> 1.0)
p1 + p2
p2 + p1

class Poly2(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly2) =
    new Poly2((other.terms foldLeft terms)(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff+"x^"+exp) mkString " + "
}

val p3 = new Poly2(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p4 = new Poly2(2 -> 4.0, 3 -> 1.0)
p3 + p4
p4 + p3
