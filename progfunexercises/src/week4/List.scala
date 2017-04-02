package week4

/**
  * Created by oliviachisman on 1/5/17.
  */

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def nth[T](n: Int, list: List[T]): T =
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) list.head
    else nth(n - 1, list.tail)

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
}

class Nil[T] extends List[T] {
  override def isEmpty = true

  override def head = throw new NoSuchElementException("Nil.head")

  override def tail = throw new NoSuchElementException("Nil.tail")
}



