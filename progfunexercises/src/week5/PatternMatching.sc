object PatternMatching {

  trait Expr {
    def apply: Expr
  }

  case class Number(n: Int) extends Expr {
    def apply = new Number(n)
  }

  case class Sum(e1: Expr, e2: Expr) extends Expr {
    def apply = new Sum(e1, e2)
  }

  case class Var(x: Int) extends Expr {
    def apply = new Var(x)
  }

  case class Prod(e1: Expr, e2: Expr) extends Expr {
    def apply = new Prod(e1, e2)
  }

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Var(x) => x
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Var(x) => x.toString
    case Prod(e1, e2) => showParens(e1) + " * " + show(e2)
  }

  def showParens(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => "(" + show(Sum(e1, e2)) + ")"
    case Var(x) => x.toString
    case Prod(e1, e2) => showParens(e1) + " * " + show(e2)
  }

  show(Prod(Sum(Number(3), Number(8)), Number(8)))

  show(Prod(Number(8), Sum(Number(3), Number(8))))

}


