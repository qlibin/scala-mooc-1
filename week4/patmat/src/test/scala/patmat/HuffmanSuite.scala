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
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times calculates correct values") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("hoffman tree computed correctly") {
    assert(createCodeTree(string2Chars("bbc")) === Fork(Leaf('c',1),Leaf('b',2),List('c', 'b'),3))
  }


  test("decode secret") {
    assert(String.valueOf(decodedSecret.toArray) === "huffmanestcool")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode `huffmanestcool` should be identity") {
    assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
  }

  test("test codeBits") {
    val codeTable: CodeTable = List(('a', List(1,0)), ('b', List(0,1)), ('c', List(1,1)))
    def testCodeBits: Char => List[Bit] = codeBits(codeTable)
    assert(testCodeBits('b') === List(0, 1))
  }

  test("test convert") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
    }
  }

  test("quickEncode works") {
    assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
  }

  test("singleton works for non empty list") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
      assert(singleton(List(t1)) === true)
    }
  }

  test("singleton on nil") {
    new TestTrees {
      assert(singleton(Nil) === false)
    }
  }

}
