package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val someText1 = "Well it was a dark night in Georgia"
    val helloWorld = "Hello World"
	}


  test("weight of a larger tree t1") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger tree t2") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times1") {
    new TestTrees {
      assert(times(string2Chars(someText1)) ===
        List((' ', 7), ('G', 1), ('W', 1), ('a', 4), ('d', 1), ('e', 2), ('g', 2), ('h', 1), ('i', 4), ('k', 1), ('l', 2), ('n', 2), ('o', 1), ('r', 2), ('s', 1), ('t', 2), ('w',1)))
    }
  }

  test("times2") {
    new TestTrees {
      assert(times(string2Chars(helloWorld)) === List((' ',1), ('H',1), ('W',1), ('d',1), ('e',1), ('l',3), ('o',2), ('r',1)))
    }
  }

  test("string2chars(\"hello, world\")") {
    new TestTrees {
      assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList larger") {
    new TestTrees {
      assert(makeOrderedLeafList(times(string2Chars(someText1))) ===
        List(Leaf('G',1), Leaf('W',1), Leaf('d',1), Leaf('h',1), Leaf('k',1), Leaf('o',1), Leaf('s',1), Leaf('w',1), Leaf('e',2), Leaf('g',2), Leaf('l',2), Leaf('n',2), Leaf('r',2), Leaf('t',2), Leaf('a',4), Leaf('i',4), Leaf(' ',7)))
    }
  }

  test("the singleton") {
    new TestTrees {
      assert(singleton(List(t1)))
      assert(!singleton(List(t1,t2)))
    }
  }

  test("combine of some leaf list insert at head") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list insert at interior") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('y', 6))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5), Leaf('y', 6)))
  }

  test("combine of some leaf list insert at end") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("until") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('y', 6))
    assert(until(singleton, combine)(leaflist) === Fork(Leaf('y',6),Fork(Leaf('x',4),Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5),List('x', 'e', 't'),9),List('y', 'x', 'e', 't'),15))
  }

  test("for createCode Tree \"hello, world\"") {
    new TestTrees {
      assert(createCodeTree(helloWorld.toList) ===
        Fork(Fork(Fork(Leaf('e',1),Leaf('r',1),List('e', 'r'),2),Fork(Leaf('W',1),Leaf('d',1),List('W', 'd'),2),List('e', 'r', 'W', 'd'),4),Fork(Leaf('l',3),Fork(Fork(Leaf(' ',1),Leaf('H',1),List(' ', 'H'),2),Leaf('o',2),List(' ', 'H', 'o'),4),List('l', ' ', 'H', 'o'),7),List('e', 'r', 'W', 'd', 'l', ' ', 'H', 'o'),11))
    }
  }

  test("Encode/Decode \"hello, world\"") {
    new TestTrees {
      val codeTree: CodeTree = createCodeTree(helloWorld.toList)
      val theBits: List[Bit] = encode(codeTree)(helloWorld.toList)
      assert(decode(codeTree, theBits).mkString === helloWorld)
    }
  }

  test("decode secret") {
    new TestTrees {
      assert(decode(frenchCode, secret).mkString === "huffmanestcool")
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("first test convert") {
    new TestTrees {
      val codeTree: CodeTree = createCodeTree(helloWorld.toList)

      val encoded: List[Bit] = quickEncode(codeTree)(helloWorld.toList)


      assert(decode(codeTree, encoded).mkString === helloWorld)
    }

  }

}
