package idealized.scala

import org.scalatest.FunSuite

/**
  * Created by wrober003c on 1/25/17.
  */
class SuccTest extends FunSuite {

  test("Zero is zero") {
    assert(Zero.isZero)
    assert(!Zero.successor.isZero)

    val one = Zero.successor
    assert(!one.successor.isZero)
    assert(one.predecessor.isZero)

    assert((Zero + one) == one)

    val two = one.successor
    assert(two.predecessor == one)

    val three = two.successor
    assert(three.predecessor == two)
    assert(three.predecessor.predecessor == one)

    val five = two + three
    assert(five.predecessor.predecessor == three)

    assert((five - two) == three)
  }
}
