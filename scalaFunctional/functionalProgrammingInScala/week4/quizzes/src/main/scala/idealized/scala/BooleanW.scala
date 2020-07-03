package idealized.scala

/**
  * Created by wrober003c on 1/24/17.
  */

abstract class BooleanW {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => BooleanW): BooleanW = ifThenElse[BooleanW](x, false1)
  def || (x: => BooleanW): BooleanW = ifThenElse[BooleanW](true1,x)
  def unary_! : BooleanW = ifThenElse[BooleanW](false1, true1)
  def == (x: BooleanW): BooleanW = ifThenElse(x, x.unary_!)
  def != (x: BooleanW): BooleanW = ifThenElse(x.unary_!, x)
  def < (x: BooleanW): BooleanW = ifThenElse(false1, x)
}


object true1 extends BooleanW {
  override def ifThenElse[T](t: => T, e: => T) = t
}

object false1 extends BooleanW {
    override def ifThenElse[T](t: => T, e: => T) = e
}