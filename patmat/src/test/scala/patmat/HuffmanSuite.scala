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
    assert(times(string2Chars("hello")).diff(List(('o',1), ('l',2), ('e',1), ('h',1))).isEmpty)
  }
  
  test("times(aaaaabbbbbccccceeeee)") {
    assert(List(('e',5), ('c',5), ('b',5), ('a',5)).diff(times(string2Chars("aaaaabbbbbccccceeeee"))).isEmpty)
  }
  

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("makeOrderedLeafList for some larger frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3), ('a', 2), ('e', 6), ('x', 10), ('a', 2), ('f', 1), ('z', 0))) === List(Leaf('z',0), Leaf('e',1), Leaf('f',1), Leaf('t',2), Leaf('a',2), Leaf('a',2), Leaf('x',3), Leaf('e',6), Leaf('x',10)))
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
  
  test("combine of some leaf list with duplicates") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('e', 6), Leaf('x', 9), Leaf('t', 10))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5), Leaf('e', 6), Leaf('x', 9), Leaf('t',10)))
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
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))
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
  
  test("createCodeTree: hello") {
    val codeTree = createCodeTree(string2Chars("hello"))
    assert(List('o', 'e', 'h', 'l').diff(chars(codeTree)).isEmpty)
    assert(5 == weight(codeTree))
  }
  
  test("times: dcccadddbccbdddcccbaccdbaccddbbccddbaccbaddd") {
    val freqs = times(string2Chars("dcccadddbccbdddcccbaccdbaccddbbccddbaccbaddd"))
    assert(List(('c',16), ('d',15), ('a',5), ('b',8)).diff(freqs).isEmpty)
  }
  
  test("createCodeTree: dcccadddbccbdddcccbaccdbaccddbbccddbaccbaddd") {
    val codeTree = createCodeTree(string2Chars("dcccadddbccbdddcccbaccdbaccddbbccddbaccbaddd"))
    assert(List('c', 'd', 'a', 'b').diff(chars(codeTree)).isEmpty)
    assert(44 == weight(codeTree))
  }
  
  test("decode: hello") {
    val codeTree = createCodeTree(string2Chars("hello"))
    System.out.println(print(codeTree))
    val chars = decode(codeTree, List(1, 0, 0, 1, 1, 1, 1, 1, 0, 0))
    assert(chars === List('h', 'e', 'l', 'l', 'o'))
  }
  
  test("french secret") {
    System.out.println(Huffman.decodedSecret)
  }

  test("encode: hello") {
    val codeTree = createCodeTree(string2Chars("hello"))
    assert(List(1, 0, 0, 1, 1, 1, 1, 1, 0, 0) === encode(codeTree)(string2Chars("hello")))
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("decode and encode french secret") {
    new TestTrees {
      assert(decode(Huffman.frenchCode, encode(Huffman.frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }
  
  test("codeBits") {
    val helloCodeTable = List(('h', List(1, 0)), ('e', List(0, 1)), ('l', List(1, 1)), ('o', List(0, 0)))
    assert(List(1, 0) === codeBits(helloCodeTable)('h'))
    assert(List(0, 1) === codeBits(helloCodeTable)('e'))
    assert(List(1, 1) === codeBits(helloCodeTable)('l'))
    assert(List(0, 0) === codeBits(helloCodeTable)('o'))
  }
  
  test("mergeCodeTables") {
    val helloCodeTable = List(('o', List(0, 0)), ('e', List(0, 1)), ('h', List(1, 0)), ('l', List(1, 1)))
    val leftTree = List(('o', List(0)), ('e', List(1)))
    val rightTree = List(('h', List(0)), ('l', List(1)))
    assert(helloCodeTable === mergeCodeTables(leftTree, rightTree))
  }
  
  test("convert to codeTable") {
    val helloCodeTable = List(('o', List(0, 0)), ('e', List(0, 1)), ('h', List(1, 0)), ('l', List(1, 1)))
    assert(helloCodeTable === convert(createCodeTree(string2Chars("hello"))))
  }
  
  test("quickDecode") {
    val codeTree = createCodeTree(string2Chars("hello"))
    val encoded = encode(codeTree)("hello".toList)
    val quickEncoded = quickEncode(codeTree)("hello".toList)
    assert(encoded == quickEncoded)
  }
}
