class Poly(val terms: Map[Int, Double]) {
  def + (other: Poly) = new Poly(terms ++ other.terms)
  override def toString: String = {
    def toStringHelper(termsTemp: Map[Int, Double]): String = termsTemp.headOption match {
      case None => ""
      case (ex, coef) =>
        if (terms.size == 1) coef + "x^" + ex
        else coef + "x^" + ex + " + " + terms.tail.toString
    }
    toStringHelper(terms)
  }
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))

p1.toString



