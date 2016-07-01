class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg() =
    new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this.less(that)) that else this

  def min(that: Rational) =
    if (this.less(that)) this else that

  override def toString = s"$numer/$denom"

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.sub(y).sub(z)

new Rational(21, 49)

//val strange = new Rational(1, 0)
//strange.add(strange)

new Rational(2)
