object session {
  class Rational(x: Int, y: Int) {
    require(y != 0, "Denominator must be non-zero")

    def this(x: Int) = this(x, 1)
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)
    val numer = x / g
    val denom = y / g

    def + (x: Rational): Rational = {
      new Rational(
        numer * x.denom + x.numer * denom,
        denom * x.denom
      )
    }

    override def toString = numer + "/" + denom
    def unary_- : Rational = new Rational(-numer, denom)
    def - (that: Rational) = this + -that
    def < (that: Rational) = this.numer * that.denom < that.numer * this.denom
    def max(that: Rational) = if (this < that) that else this
  }

  var x = new Rational(1, 3)
  var y = new Rational(5, 7)
  var z = new Rational(3, 2)
  x - y - z
  y + y
  x < y
  y < x
  x.max(y)
  var w = new Rational(2)
}

