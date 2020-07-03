import java.util

object rationals {
	val xf = new Rational(2, 1)
	xf.numer
	xf.denom

	def addRational(r: Rational, s: Rational): Rational =
		new Rational(
			r.numer * s.denom + s.numer * r.denom,
			r.denom * s.denom)

	def makeString(r: Rational) =
		r.numer + "/" + r.denom

	makeString(addRational(new Rational(1, 2), new Rational(2, 3)))

	val yf = new Rational(2, 3)
	xf.add(yf)

	val x = new Rational(1,3)
	val y = new Rational(5,7)
	val z = new Rational(3,2)

	x.add(y).mul(z)
	x.neg
	x.sub(y).sub(z)

	class Rational(numerator: Int, denominator: Int) {
		def numer = numerator
		def denom = denominator

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

		override def toString = numer + "/" + denom

	}

}


