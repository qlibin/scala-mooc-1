trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }
}

case class Number(n: Int) extends Expr
case class Var(name: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

show(Prod(Number(2), Number(8)))

val sum_1_2 = Sum(Number(1), Number(2))

sum_1_2.eval
val sum_1_2_5 = Sum(sum_1_2, Number(5))

show(sum_1_2_5)
sum_1_2_5.eval


def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Var(name) => name
  case Sum(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
  case Prod(e1, e2) => show(e1) + " * " + show(e2)
}

show(Sum(Prod(Number(2), Var("x")), Var("y")))

show(Prod(Sum(Number(2), Var("x")), Var("y")))