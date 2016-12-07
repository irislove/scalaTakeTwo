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
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  
  test("times(List('h', 'e', 'l', 'l', 'o'))") {
    assert(times(string2Chars("hello")) === List(('o',1), ('l',2), ('e',1), ('h',1)))
  }
  

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton list:List(Leaf('e', 1)") {
    val leaflist = List(Leaf('e', 1))
    assert(true == singleton(leaflist))
  }

  test("not a singleton list:List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(false == singleton(leaflist))
  }
  

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine of some fork list") {
    val leaflist = List(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Fork(Leaf('c',1), Leaf('d',5), List('c','d'), 6), Fork(Leaf('e',6), Leaf('f',7), List('e','f'), 13))
    assert(combine(leaflist) === List(
        Fork(
            Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),
            Fork(Leaf('c',1), Leaf('d',5), List('c','d'), 6), 
            List('a', 'b', 'c', 'd'),
            11), 
        Fork(Leaf('e',6), Leaf('f',7), List('e','f'), 13)
    ))
  }
  
  test("combine only two elements") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Leaf('e', 1), Leaf('t', 2)))
  }
  
  test("combine of some fork and leaf list") {
    val leaflist = List(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('c', 1), Fork(Leaf('e',6), Leaf('f',7), List('e','f'), 13))
    assert(combine(leaflist) === List(
        Fork(
            Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),
            Leaf('c', 1), 
            List('a', 'b', 'c'),
            6), 
        Fork(Leaf('e',6), Leaf('f',7), List('e','f'), 13)
    ))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
