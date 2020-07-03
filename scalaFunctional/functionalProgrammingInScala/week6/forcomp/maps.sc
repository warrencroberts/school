val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry("US")
//capitalOfCountry("blargo")

capitalOfCountry get "US"

capitalOfCountry get "blargo"

def showCapital(country: String ) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("US")
showCapital("blargo")

val fruit = List("apple","pear","orange","pineapple")
fruit sortWith(_.length < _.length)
fruit.sorted

fruit groupBy (_.head)

// polinomial x^3 - 2x + 5
Map(0 -> 5, 1 -> -2, 3 -> 1)

class Poly(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms: Map[Int, Double] = terms0 withDefaultValue 0.0
//  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
//  def adjust(term: (Int, Double)): (Int, Double) = {
//    val (exp, coeff) = term
//    exp -> (coeff + terms(exp))
//  }


  def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString: String =
    (for((exp, coeff) <- terms.toList.sorted.reverse) yield s"${coeff}x^$exp") mkString " + "

}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

p1 + p2
p1.terms(7)




