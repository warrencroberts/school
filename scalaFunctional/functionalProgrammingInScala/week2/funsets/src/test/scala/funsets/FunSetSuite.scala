package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val trans_s1 = singletonSet(2)

    val s2 = singletonSet(2)
    val trans_s2 = singletonSet(4)

    val s3 = singletonSet(3)
    val trans_s3 = singletonSet(6)

    val s4 = singletonSet(4)
    val trans_s4 = singletonSet(8)

    val unionSet = union(union(union(s1, s2), s3), s4)
    val transUnionSet = union(union(union(trans_s1, trans_s2), trans_s3), trans_s4)
    val intersectedSet = intersect(unionSet, union(s2, s3))
    val diffSet = diff(unionSet, union(s2, s3))

    printSet("unionSet : ", unionSet)
    printSet("transUnionSet : ", transUnionSet)
    printSet("intersectedSet : ", intersectedSet)
    printSet("diffSet : ", diffSet)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(unionSet, 1), "Union 1")
      assert(contains(unionSet, 2), "Union 2")
      assert(contains(unionSet, 3), "Union 3")
      assert(contains(unionSet, 4), "Union 4")
    }
  }

  test("intersect contains only elements common to both sets") {
    new TestSets {
      assert(contains(intersectedSet, 2), "Intersect {1,2,3,4} {2,3} should contain 2")
      assert(!contains(intersectedSet, 1), "Intersect {1,2,3,4} {2,3} should not contain 1")
      assert(contains(intersectedSet, 3), "Intersect {1,2,3,4} {2,3} should contain 3")
      assert(!contains(intersectedSet, 4), "Intersect {1,2,3,4} {2,3} should not contain 4")
    }
  }

  test("diff contains elements from set 1 that aren't in set 2") {
    new TestSets {
      assert(contains(diffSet, 1), "Intersect {1,2) {2,3} should contain 1")
      assert(!contains(diffSet, 2), "Intersect {1,2) {2,3} should not contain 2")
      assert(!contains(diffSet, 3), "Intersect {1,2) {2,3} should not contain 3")
    }
  }

  test("filter contains elements from set 1 that match filter function") {
    new TestSets {
      val filteredSet = filter(unionSet, x => x != 2)
      printSet("filteredSet : ", filteredSet)

      assert(contains(filteredSet, 1), "filtered set should contain 1")
      assert(contains(filteredSet, 3), "filtered set should contain 3")
      assert(!contains(filteredSet, 2), "filtered set should not contain 2")
    }
  }

  test("for all tests whether all integers in a set match the given predicate") {
    new TestSets {
      assert(forall(unionSet, x => x <= 4), "all members of set should be < 4")
      assert(!forall(unionSet, x => x < 4), "member 3 doesn't match predicate")
      assert(forall(unionSet, x => x > 0), "all members of set should be > 0")
      assert(!forall(unionSet, x => x > 1), "member 1 doesn't match prdicate")

      val allMembersFn = (x: Int) => -bound to bound contains x
      assert(forall(union(unionSet, singletonSet(bound)), allMembersFn))
      assert(!forall(union(unionSet, singletonSet(bound + 1)), allMembersFn))
    }
  }

  test("map returns a set where each member has function f applied") {
    new TestSets {
      println("------------------ results start here ------------------")
      val retSet = map(unionSet, x => x * 2)
      printSet("returned from map : ", retSet)

      assert(forall(retSet, (x: Int) => contains(transUnionSet, x)))
    }
  }
}
