package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    val aSig = a()
    val bSig = b()
    val cSig = c()

    Var((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    delta() match {
      case x if x < 0 => Var(Set())
      case x if x == 0 => Var(Set(-b()/(2 * a())))
      case x => Var(Set((-b() + Math.sqrt(b())) / (2 * a()), (-b() - Math.sqrt(b())) / (2 * a())))
    }
  }
}
