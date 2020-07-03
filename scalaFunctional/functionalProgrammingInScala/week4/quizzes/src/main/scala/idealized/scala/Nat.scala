package idealized.scala

/**
  * Created by wrober003c on 1/24/17.
  */
abstract class Nat {
  def isZero : Boolean
  def predecessor : Nat
  def successor : Nat
  def + (that: Nat) : Nat
  def - (that: Nat) : Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new NumberFormatException("Natural number cannot be < 0")
  def successor : Nat = Succ(this)
  def + (that: Nat) : Nat = that
  def - (that: Nat) : Nat = if(that.isZero) this else throw new NumberFormatException("Natural number cannot be < 0")
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor : Nat = Succ(this)
  def + (that: Nat) : Nat = n + that.successor

  def - (that: Nat) : Nat =
    if(that == Zero)
      this
    else
      n - that.predecessor
}

object Succ {
  def apply(n: Nat): Succ =  new Succ(n)
}