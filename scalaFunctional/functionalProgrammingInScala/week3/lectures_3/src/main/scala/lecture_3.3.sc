import week3.{Cons, Nil, List}

//trait List[T] {
//  def isEmpty: Boolean
//  def head: T
//  def tail: List[T]
//}
//
//class Cons[T](val head: T, val tail: List[T]) extends List[T] {
//  def isEmpty = false
//}
//
//class Nil[T] extends List[T] {
//  def isEmpty: Boolean = true
//  def head: Nothing = throw new NoSuchElementException("Nil Head")
//  def tail: Nothing = throw new NoSuchElementException("Nil Tail")
//}

def nth[T] (n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException("out of bounds")
  else if(n == 0) xs.head
  else nth(n-1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(2, list)