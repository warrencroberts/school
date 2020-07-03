package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
//    namedExpressions.foreach(r => println(s"computeValues-namedExpressions : ${r._1} = ${r._2()}"))

    val theValues = namedExpressions.foldLeft(Map[String, Signal[Double]]()) { (x, e) =>
      val ourExpression = namedExpressions(e._1)
      x + (e._1 -> Signal(eval(e._2(), namedExpressions))) //<----- need to pass function in here so that it can get the value
    }

//    theValues.foreach(r => println(s"computeValues-theValues : ${r._1} = ${r._2()}"))
    theValues
}

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
//    println(s"eval: expr = $expr,  size: ${references.size}")
//    references.foreach(r => println(s"eval: ${r._1} -> ${r._2()}"))

    expr match {
      case Literal(x) => x
      case Ref(name) =>
        eval(getReferenceExpr(name, references), references - name)

      case Plus(x,y) =>
        eval(x, references) + eval(y, references)

      case Minus(x,y) => eval(x, references) - eval(y, references)
      case Times(x,y) => eval(x, references) * eval(y, references)
      case Divide(x,y) => eval(x, references) / eval(y, references)
      case _ => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
//    println(s"getReferenceExpr : name = $name size = ${references.size}")
//    references.foreach(r => println(s"getReferenceExpr: ${r._1} -> ${r._2()}"))

    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
