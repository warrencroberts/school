// Rational numbers

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg : Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = numer + "/" + denom
}

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

x.add(new Rational(2,1))

x.sub(y).sub(z)

class Rational1(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a%b)

  def numer = x
  def denom = y

  def +(that: Rational1) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational1 = new Rational1(-numer, denom)

  def - (that: Rational1) = this + -that

  def <(that: Rational1) = numer * that.denom < that.numer * denom
  def max(that: Rational1) = if(this < that)that else this

//  override def toString = numer + "/" + denom
  override def toString = {
    val d = gcd(x,y)
    (numer / d) + "/" + (denom / d)
  }
}

val w1 = new Rational1(8,10)
val x1 = new Rational1(1,3)
val y1 = new Rational1(5,7)
val z1 = new Rational1(3,2)

x1 < y1
x1 max y1
val strange = new Rational1(1,0)
strange + strange


