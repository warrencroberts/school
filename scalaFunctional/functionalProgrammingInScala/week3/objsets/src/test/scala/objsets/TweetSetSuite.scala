package objsets

import java.util.NoSuchElementException

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val setHasLeft1 = set4d.incl(c)
    val setHasLeft2 = setHasLeft1.incl(new Tweet("a", "a body 1", 5))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: user a on setHasLeft1") {
    new TestSets {
      assert(size(setHasLeft1.filter(tw => tw.user == "c")) === 1)
    }
  }

  test("filter: user a on setHasLeft2") {
    new TestSets {
      println(setHasLeft2.toString)
      assert(size(setHasLeft2.filter(tw => (tw.user == "a") || (tw.user == "c"))) === 3)
    }
  }

  test("filter: all tweets on setHasLeft2") {
    new TestSets {
      assert(size(setHasLeft2.filter(tw => true)) === 5)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retweeted on Empty should return throw NoSuchElementException") {
    new TestSets {
      intercept[NoSuchElementException] {
        set1.mostRetweeted
      }
    }
  }

  test("most retweeted on single NonEmpty should return the element") {
    new TestSets {
      assert(set2.mostRetweeted.text == "a body")
    }
  }

  test("descending: set5") {
    new TestSets {
      println(set5.toString)
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  }
