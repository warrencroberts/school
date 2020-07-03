abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet) : IntSet
  def nth(n: Int) : Int
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def union(other: IntSet): IntSet = other
  def nth(n: Int) = throw new IndexOutOfBoundsException("Empty element doesn't have any elements")

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if( x > elem)  new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = s"{$left$elem$right}"

  def nth(n: Int): Int = {
    println(s"n = $n")
    if(n == 0) elem
    if(n < 0) throw new IndexOutOfBoundsException(s"out of bounds")
    else nth(n - 1)
  }
}

println("Welcome to the Scala worksheet")
val t1 = new NonEmpty(3, new Empty, new Empty)
val t2 = t1 incl 4

t2.nth(1)

