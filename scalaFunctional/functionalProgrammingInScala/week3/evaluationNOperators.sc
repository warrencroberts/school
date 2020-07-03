

object evaluationNOperators {
	val x = new Rational(1,3)
	val y = new Rational(5,7)
	val z = new Rational(3,2)
	x + y * z
	-x
	x - y - z
	y + y
	x < y
	x max y


	class Rational(numerator: Int, denominator: Int) {
		require(denominator > 0 , "denominator must be positive")

		def this(x: Int) = this(x,1)

		private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
		private val g = gcd(numerator,denominator)
		def numer = numerator
		def denom = denominator


		def < (that:Rational) = numer * that.denom < denom * that.denom
		def max (that: Rational) = if(this < that) that else this
		def + (that: Rational) =
			new Rational(
				numer * that.denom + that.numer * denom,
				denom * that.denom)
		def - (that: Rational) = this + -that
		def * (that: Rational) =
			new Rational(
				numer * that.numer,
				denom * that.denom)

		def unary_- : Rational =
			new Rational(
				-1 * numer,
				denom)

		override def toString = {
			val g = math.abs(gcd(numer, denom))
			println("g = " + g)
			numer / g + "/" + denom / g
		}
	}
}