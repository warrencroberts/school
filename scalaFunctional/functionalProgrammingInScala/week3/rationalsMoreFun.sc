import java.util
object rationalsMoreFun {
	val x = new Rational(1,3)
	val y = new Rational(5,7)
	val z = new Rational(3,2)
	x.add(y).mul(z)
	x.neg
	x.sub(y).sub(z)
	y.add(y)
	x.less(y)
	x.max(y)
	val strange = new Rational(1,0)
	new Rational(2)
	strange.add(strange)

	class Rational(numerator: Int, denominator: Int) {
		require(denominator > 0 , "denominator must be positive")

		def this(x: Int) = this(x,1)

		private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
		private val g = gcd(numerator,denominator)
		def numer = numerator
		def denom = denominator


		def less(that:Rational) = numer * that.denom < denom * that.denom
		def max(that: Rational) = if(this.less(that)) that else this
		def add(that: Rational) =
			new Rational(
				numer * that.denom + that.numer * denom,
				denom * that.denom)
		def sub(that: Rational) = add(that.neg)
		def mul(that: Rational) =
			new Rational(
				numer * that.numer,
				denom * that.denom)

		def neg: Rational =
			new Rational(
				-1 * numer,
				denom)

		override def toString = {
			val g = gcd(numer, denom)
			numer / g + "/" + denom / g
		}
	}

}


