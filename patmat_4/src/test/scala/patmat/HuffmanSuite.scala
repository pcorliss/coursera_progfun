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
    val l1 = Leaf('a', 2)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(Leaf('a', 2)) === 2)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
      assert(chars(l1) === List('a'))

    }
  }

  test("make code tree") {
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    )
    assert(weight(sampleTree) === 4)
    assert(chars(sampleTree) === List('x', 'e', 't'))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(string2Chars("foo")) === List(('o', 2), ('f', 1)))
  }
//
//
//  test("makeOrderedLeafList for some frequency table") {
//    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
//  }
//
//
//  test("combine of some leaf list") {
//    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
//    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
//  }
//
//
//  test("decode and encode a very short text should be identity") {
//    new TestTrees {
//      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
//    }
//  }

}
