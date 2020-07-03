package quickcheck

import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{oneOf, _}


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty),
    for {
    newVal <- arbitrary[Int]
    node <-  oneOf(const(empty), genHeap)
  } yield insert(newVal, node))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("Inserting min and then finding it as the min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  def monotonicallyIncreasing(h: H): Boolean = {
    def helper(h: H, lastMin: Int): Boolean = {
      if (isEmpty(h))
        true
      else {
        val v1 = findMin(h)
        val newHeap = deleteMin(h)
        if(isEmpty(newHeap))
          true
        else {
          val v2 = findMin(newHeap)
          if (v1 <= v2)
            helper(deleteMin(newHeap), v2)
          else
            false
        }
      }
    }

    helper(h, Int.MinValue)
  }

  property("monotonically increasing") = forAll { (h: H) =>
    monotonicallyIncreasing(h)
  }

  property("min of meld is min of input heaps") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2))
      isEmpty(meld(h1, h2))
    else {
      if(isEmpty(h1)) {
        findMin(h2) == findMin(meld(h1,h2))
      } else if(isEmpty(h2)) {
        findMin(h1) == findMin(meld(h1,h2))
      } else {
        val minH = Math.min(findMin(h1), findMin(h2))
        findMin(meld(h1,h2)) == minH
      }
    }
  }

  property("meld heaps monotonic heaps") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2))
      true
    else {
      if(isEmpty(h1)) {
        monotonicallyIncreasing(meld(h1,h2))
      } else if(isEmpty(h2)) {
        monotonicallyIncreasing(meld(h1,h2))
      } else {
        monotonicallyIncreasing(meld(h1,h2))
      }
    }
  }

  property("meld heaps monotonic heaps") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2))
      true
    else {
      if(isEmpty(h1)) {
        monotonicallyIncreasing(meld(h1,h2))
      } else if(isEmpty(h2)) {
        monotonicallyIncreasing(meld(h1,h2))
      } else {
        val minH = Math.min(findMin(h1), findMin(h2))
        monotonicallyIncreasing(meld(h1,h2))
      }
    }
  }

  def sizeOfHeap(h: H) : Int = {
    def helper(helperH: H, accum: Int): Int = {
      println(s"helperH = $helperH, accum = $accum")
      if(isEmpty(helperH))
        accum
      else {
        helper(deleteMin(helperH), accum + 1)
      }
    }

    helper(h, 0)
  }

  def heapToList(h: H) : List[A] = {
    def helper(helperH: H, accum: List[A]) : List[A] = {
      if(isEmpty(helperH)) {
        accum
      } else {
        val theVal = findMin(helperH)
        helper(deleteMin(helperH), theVal :: accum)
      }
    }
    helper(h, List[A]()).reverse
  }

  property("meld heaps is sum of heaps") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val h1List = heapToList(h1)
      val h2List = heapToList(h2)
      val testList = (h1List ++ h2List).sorted

      val heapList = meld(h1,h2)
      val meldList = heapToList(heapList)

      if(testList != meldList) {
        println(s"\ntestList : $testList")
        println(s"meldList : $meldList")
      }

      testList == meldList
    }
  }

}
