def loop :Boolean = loop
def and(x:Boolean, y: => Boolean) = if(x) y else false


and(true, true)
and(false, true)
and(true, false)
and(false, false)
and(false, loop)

def or(x:Boolean, y: => Boolean) = if(x) true else y

or(true, true)
or(false, true)
or(true, false)
or(false, false)
or(true, loop)


def sqrt(x:Double) = {
  def sqrtIter(guess: Double, x: Double): Double = {
    def abs(x: Double) = if(x < 0) -x else x

    def isGoodEnough(newGuess: Double, x: Double): Boolean = {
      abs((newGuess * newGuess) - x) / x < 0.0001
    }
    def improve(oldGuess: Double, x: Double) = (oldGuess + x / oldGuess) / 2

    if(isGoodEnough(guess, x) )
      guess
    else
      sqrtIter(improve(guess, x), x)
  }

  sqrtIter(1.0, x)
}

sqrt(2)
1.4142156862745097 * 1.4142156862745097

sqrt(0.001)
0.03162278245070105 * 0.03162278245070105

sqrt(0.1e-20)
3.1622778383672726E-11 * 3.1622778383672726E-11

sqrt(1.0e50)
1.0000003807575104E25 * 1.0000003807575104E25

def sqrt_simpler(x:Double) = {
  def sqrtIter(guess: Double): Double = {
    def abs(x: Double) = if(x < 0) -x else x

    def isGoodEnough(newGuess: Double): Boolean = {
      abs((newGuess * newGuess) - x) / x < 0.0001
    }
    def improve(oldGuess: Double) = (oldGuess + x / oldGuess) / 2

    if(isGoodEnough(guess) )
      guess
    else
      sqrtIter(improve(guess))
  }

  sqrtIter(1.0)
}

sqrt(2)
1.4142156862745097 * 1.4142156862745097

sqrt(0.001)
0.03162278245070105 * 0.03162278245070105

sqrt(0.1e-20)
3.1622778383672726E-11 * 3.1622778383672726E-11

sqrt(1.0e50)
1.0000003807575104E25 * 1.0000003807575104E25
