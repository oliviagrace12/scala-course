object intsets {

  val set6 = Empty.incl(6)
  val set7 = Empty.incl(7)
  val set67 = set6 union set7


  abstract class IntSet {
    def incl(x: Int): IntSetx

    def contains(x: Int): Boolean

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    override def contains(x: Int): Boolean = false

    override def union(other: IntSet): IntSet = other

    override def toString: String = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def contains(x: Int): Boolean = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    }

    override def incl(x: Int): IntSet = {
      if (x < elem) left incl x
      else if (x > elem) right incl x
      else this
    }

    override def union(other: IntSet): IntSet = {
      ((left union right) union other) incl elem
    }

    override def toString: String = "{" + left + elem + right +"}"
  }

}