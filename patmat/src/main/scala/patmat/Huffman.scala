package patmat

import scala.collection.immutable.{::, List}
import scala.collection.mutable.HashMap

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w
    case Leaf(_, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, c, _) => c
    case Leaf(c, _) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    * times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    * List(('a', 2), ('b', 1))
    *
    * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
    * character and an integer. Pairs can be constructed easily using parentheses:
    *
    * val pair: (Char, Int) = ('c', 1)
    *
    * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
    *
    * val theChar = pair._1
    * val theInt  = pair._2
    *
    * Another way to deconstruct a pair is using pattern matching:
    *
    * pair match {
    * case (theChar, theInt) =>
    * println("character is: "+ theChar)
    * println("integer is  : "+ theInt)
    * }
    */
  def times(chars: List[Char]): List[(Char, Int)] = {
    val charMap = new HashMap[Char, Int]

    def findTimes(charsTemp: List[Char]): Unit = {
      if (charsTemp.isEmpty) return
      val char = charsTemp.head
      charMap.put(char, charMap.getOrElse(char, 0) + 1)
      if (charsTemp.size > 1) {
        findTimes(charsTemp.tail)
      }
    }

    findTimes(chars)

    charMap.toList
  }


  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def sort(xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => List()
      case y :: ys => insert(y, sort(ys))
    }

    def insert(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => List(x)
      case y :: ys => if (x._2 >= y._2) x :: xs else y :: insert(x, ys)
    }

    def convert(pairs: List[(Char, Int)], leaves: List[Leaf]): List[Leaf] = {
      if (pairs.size == 1) {
        return Leaf(pairs.head._1, pairs.head._2) :: leaves
      }
      convert(pairs.tail, Leaf(pairs.head._1, pairs.head._2) :: leaves)
    }

    convert(sort(freqs), List())
  }

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean = trees.size == 1

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def insert(x: CodeTree, xs: List[CodeTree]): List[CodeTree] = xs match {
      case List() => List(x)
      case y :: ys => if (weight(x) <= weight(y)) x :: xs else y :: insert(x, ys)
    }

    if (trees.size <= 1) trees
    else {
      val fork = makeCodeTree(trees.head, trees.tail.head)
      insert(fork, trees.tail.tail)
    }
  }

  /**
    * This function will be called in the following way:
    *
    * until(singleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
    * the two functions defined above.
    *
    * In such an invocation, `until` should call the two functions until the list of
    * code trees contains only one single tree, and then return that singleton list.
    *
    * Hint: before writing the implementation,
    *  - start by defining the parameter types such that the above example invocation
    * is valid. The parameter types of `until` should match the argument types of
    * the example invocation. Also define the return type of the `until` function.
    *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
    */
  def until(requirement: List[CodeTree] => Boolean, action: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (requirement(trees)) trees else until(requirement, action)(action(trees))
  }

  /**
    * This function creates a code tree which is optimal to encode the text `chars`.
    *
    * The parameter `chars` is an arbitrary text. This function extracts the character
    * frequencies from that text and creates a code tree based on them.
    */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  // Part 3: Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeStep(subTree: CodeTree, bitsTemp: List[Bit], chars: List[Char]): List[Char] = subTree match {
      case Fork(l, r, _, _) =>
        println("** FORK ")
        println("   BITS (" + bitsTemp.size + "): " + bitsTemp)
        println("   CHARS: " + chars)
        if (bitsTemp.isEmpty) chars reverse
        else if (bitsTemp.head == 0) {
          println("   Current bit: " + bitsTemp.head)
          decodeStep(l, bitsTemp.tail, chars)
        }
        else {
          println("   Current bit: " + bitsTemp.head)
          decodeStep(r, bitsTemp.tail, chars)
        }
      case Leaf(c, _) =>
        println("**** LEAF ")
        println("   Adding " + c + " to chars")
        println("   BITS (" + bitsTemp.size + "): " + bitsTemp)
        if (bitsTemp.size < 1) (c :: chars) reverse
        else {
          println("   Current bit: " + bitsTemp.head)
          decodeStep(tree, bitsTemp, c :: chars)
        }
    }

    decodeStep(tree, bits, List[Char]())
  }

  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the 'frenchCode' Huffman tree defined above.
    **/
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = {
    println("Encoded message: " + secret)
    println("first element: " + secret.head)

    def message = decode(frenchCode, secret)

    println("Decoded message: " + message.toString())
    message
  }


  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  // Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeStep(treeTemp: CodeTree, bits: List[Bit])(textTemp: List[Char]): List[Bit] = treeTemp match {
      case Leaf(c, _) =>
        println("LEAF")
        println("Bits: " + bits + " for " + c)
        println("text: " + textTemp)
        if (c != textTemp.head) throw new Error("Text contains character not in code tree")
        else if (textTemp.size > 1) encodeStep(tree, bits)(textTemp.tail)
        else bits reverse
      case Fork(l, r, _, _) =>
        println("FORK")
        println("   Current char: " + textTemp.head)
        println("   Bits: " + bits)
        if (chars(l) contains textTemp.head) {
          println("   Found correct branch: " + chars(l))
          encodeStep(l, 0 :: bits)(textTemp)
        }
        else if (chars(r) contains textTemp.head) {
          println("   Found correct branch: " + chars(r))
          encodeStep(r, 1 :: bits)(textTemp)
        }
        else throw new Error("Text contains character not in code tree")
    }

    println("Text: " + text)
    encodeStep(tree, List[Int]())(text)
  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */

  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    def error = throw new Error("Character " + char + " not found in table")

    if (table.isEmpty) error
    else if (table.head._1 == char) table.head._2
    else if (table.size == 1) error
    else codeBits(table.tail)(char)
  }

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = {
    def encodeChars(chars: List[Char], table: CodeTable): CodeTable = {
      println("chars: " + chars)
      println("table progress: " + table)
      if (chars.size == 1) {
        val code = encode(tree)(List(chars.head))
        val finalTable = (chars.head, code) :: table
        println("Final table: " + finalTable)
        finalTable
      } else {
        println("table still more to go: " + table)
        encodeChars(chars.tail, (chars.head, encode(tree)(List(chars.head))) :: table)
      }
    }

    println(chars(tree))

    encodeChars(chars(tree), List())
  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {a ::: b}

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    text flatMap codeBits(convert(tree))
  }
}
