package forcomp

import java.io.Serializable

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Anagrams.{Occurrences, _}

import scala.collection.immutable.IndexedSeq

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: middle decrement") {
    val lard = List(('a', 1), ('d', 1), ('l', 2), ('r', 1))
    val r = List(('l', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: middle remove") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('l', 1))
    val lad = List(('a', 1), ('d', 1), ('r', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: head remove") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('a', 1))
    val lad = List(('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: head decrement") {
    val lard = List(('a', 2), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('a', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard, r) === lad)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )

    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: abba-3") {
    val abba = List(('a', 2), ('b', 3), ('c', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),

      List(('b', 1)),
      List(('b', 2)),
      List(('b', 3)),
      List(('c', 1)),

      List(('c', 2)),

      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 2)),
      List(('a', 1), ('b', 3)),
      List(('a', 1), ('c', 1)),
      List(('a', 1), ('c', 2)),

      List(('a', 2), ('b', 1)),
      List(('a', 2), ('b', 2)),
      List(('a', 2), ('b', 3)),
      List(('a', 2), ('c', 1)),
      List(('a', 2), ('c', 2)),

      List(('b', 1), ('c', 1)),
      List(('b', 1), ('c', 2)),

      List(('b', 2), ('c', 1)),
      List(('b', 2), ('c', 2)),

      List(('b', 3), ('c', 1)),
      List(('b', 3), ('c', 2)),

      List(('a', 1), ('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 2)),
      List(('a', 1), ('b', 2), ('c', 1)),
      List(('a', 1), ('b', 2), ('c', 2)),
      List(('a', 1), ('b', 3), ('c', 1)),
      List(('a', 1), ('b', 3), ('c', 2)),

      List(('a', 2), ('b', 1), ('c', 1)),
      List(('a', 2), ('b', 1), ('c', 2)),
      List(('a', 2), ('b', 2), ('c', 1)),
      List(('a', 2), ('b', 2), ('c', 2)),
      List(('a', 2), ('b', 3), ('c', 1)),
      List(('a', 2), ('b', 3), ('c', 2))
    )

    val firstSort = (for {
      (x, l) <- combinations(abba).groupBy(x => x.length)
    } yield l.sortBy(an => (an.head._1, an.head._2))).toList
        .sortBy(x => x.head.length).foldLeft(List[Occurrences]())((l, occ) => l ++ occ)

    val secondSort = (for {
      (x, l) <- abbacomb.groupBy(x => x.length)
    } yield l.sortBy(an => (an.head._1, an.head._2))).toList
      .sortBy(x => x.head.length).foldLeft(List[Occurrences]())((x,y) => x ++ y)

    println(firstSort.mkString("\n"))
    println("-------------------")
    println(secondSort.mkString("\n"))

    assert(firstSort.toSet === secondSort.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
