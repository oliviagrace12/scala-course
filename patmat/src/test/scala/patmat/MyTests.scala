package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class MyTests extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  trait LeafList {
    val leafList: List[Leaf] = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('r', 3), Leaf('w', 1))
  }

  trait TestTrees2 {
    val charWeights: List[(Char, Int)] = List(('e', 1), ('t', 2), ('x', 4), ('r', 3), ('w', 1))
    val codeTree: CodeTree = until(singleton, combine)(makeOrderedLeafList(charWeights)).head

  }


  test("weight") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }


  test("chars") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
    }
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e', 1)))
  }


  test("create a CodeTree from an ordered list of leaves") {
    new LeafList {
      assert(until(singleton, combine)(leafList) === List(
        Fork(
          Fork(
            Leaf('r', 3),
            Leaf('w', 1), List('r', 'w'), 4),
          Fork(
            Fork(
              Leaf('e', 1),
              Leaf('t', 2), List('e', 't'), 3),
            Leaf('x', 4), List('e', 't', 'x'), 7), List('r', 'w', 'e', 't', 'x'), 11)))
    }
  }

  test("decode should decode the message") {
    new TestTrees2 {
      println("Code tree: " + codeTree)
      val decoded: List[Char] = decode(codeTree, List(0, 0, 0, 1, 0, 1, 0))
      println("Decoded message: " + decoded)
      assert(decoded === List('x', 'x', 'x', 'r', 'r'))
    }
  }

  test("decode should decode the french message") {
    val decoded: List[Char] = decode(frenchCode, secret)
    println("Decoded message: " + decoded)
    assert(decoded === string2Chars("huffmanestcool"))
  }

  test("decode should decode the french message 2") {
    val decoded: List[Char] = decodedSecret
    println("Decoded message: " + decoded)
    assert(decoded === string2Chars("huffmanestcool"))
  }

  test("codeBits should return the bit sequence for a given character") {
    val codeTable = List(('x', List(0, 1, 0)))

    assert(codeBits(codeTable)('x') === List(0, 1, 0))
  }

  test("codeBits should return the bit sequence for a given character from a list of many") {
    val codeTable: CodeTable = List(('a', List(1, 0, 0)), ('b', List(0,1,0)), ('c', List(0,0,1)))

    assert(codeBits(codeTable)('a') === List(1, 0, 0))
    assert(codeBits(codeTable)('b') === List(0, 1, 0))
    assert(codeBits(codeTable)('c') === List(0, 0, 1))
  }

  test("convert should create a code table out of a code tree") {
    new TestTrees2 {
      val codeTable: CodeTable = convert(codeTree)
      assert(codeBits(codeTable)('r') === List(1,0))
      assert(codeBits(codeTable)('t') === List(1,1,1))
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }


}
