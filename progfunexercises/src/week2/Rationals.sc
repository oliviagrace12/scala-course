object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  val t = new Rational(2)

  // ((a + b) ^? (c ?^ d)) less ((a ==> b) | c)

  x.numer
  x.add(y)
  y

  x.sub(y).sub(z).g
  x.less(y)
  x.max(z)

  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be positive")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
    val g = gcd(x, y)
    def numer = x / g
    def denom = y / g

    def less(that: Rational) = numer * that.denom < that.numer * denom

    def max(that: Rational) =
      if (less(that)) that
      else this

    def add(that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    def neg = new Rational(-numer, denom)

    def sub(that: Rational) = add(that.neg)

    override def toString = numer + "/" + denom
  }
}


