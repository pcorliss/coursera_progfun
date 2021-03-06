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


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton list") {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val doub = List(t1, t2)
    val single = List(t1)
    val empty = List()
    assert(singleton(doub) === false)
    assert(singleton(single) === true)
    assert(singleton(empty) === false)
  }

  test("combine of some leaf list") {
    // Returns list unchanged if < 2 elements
    val empty = List()
    val single = List(Leaf('a',2))
    assert(combine(empty) == empty)
    assert(combine(single) == single)

    // Takes list of trees ascending weights
    // Returns first two elements combined into a Fork
    val pair = List(Leaf('a', 1), Leaf('b', 2))
    val combined = combine(pair)

    assert(singleton(combined))
    assert(combined == List(Fork(Leaf('a',1),Leaf('b',2), List('a','b'),3)))

    //Inserted into a longer ascending weight list
    val ll1 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(ll1) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))

    //Inserted into a list with sorting maintained
    val ll2 = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 3))
    assert(combine(ll2) === List(Leaf('x',3), Fork(Leaf('e',2),Leaf('t',2),List('e', 't'),4)))
  }

  test("until executes until true") {
    val trees = List(Leaf('a', 2), Leaf('b', 3), Leaf('d', 4))
    val merged = until(singleton, combine)(trees)
    val expected = List(Fork(Leaf('d',4),Fork(Leaf('a',2),Leaf('b',3),List('a', 'b'),5),List('d', 'a', 'b'),9))
    assert(singleton(merged))
    assert(merged === expected)
  }

  test("create code tree") {
    val t1 = createCodeTree(string2Chars("eagle"))
    assert(chars(t1) === List('e', 'l', 'g', 'a'))
    assert(weight(t1) === 5)
    assert(t1 === Fork(Leaf('e',2),Fork(Leaf('l',1),Fork(Leaf('g',1),Leaf('a',1),List('g', 'a'),2),List('l', 'g', 'a'),3),List('e', 'l', 'g', 'a'),5))
  }

  test("decoded secret") {
    assert(decodedSecret === string2Chars("huffmanestcool"))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(encode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }

  test("codeBits Lookup") {
    val cb = List(('a', List(1, 0)), ('b', List(0)), ('c', List(1)))
    assert(codeBits(cb)('a') === List(1, 0))
    assert(codeBits(cb)('b') === List(0))
    assert(codeBits(cb)('c') === List(1))
  }

  test("convert tree to code table") {
    new TestTrees {
      val convertedT1 = convert(t1)
      assert(codeBits(convertedT1)('a') === List(0))
      assert(codeBits(convertedT1)('b') === List(1))
      val frenchTable = convert(frenchCode)
      assert(codeBits(frenchTable)('h') === List(0,0,1,1,1,0,1))
      assert(codeBits(frenchTable)('u') === List(0,1,1,1))
      assert(codeBits(frenchTable)('f') === List(0,0,1,1,0,1))
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }
}
