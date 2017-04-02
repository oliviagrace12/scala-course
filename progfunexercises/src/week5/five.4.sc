import scala.collection.immutable.::

def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

squareList(List(1,2,3))

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x * x)

squareList2(List(8,2,3))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (a => a == x)
    first :: pack(rest)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (a => a == x)
    (first.head, first.size) :: encode(rest)
}

encode(List("a", "a", "a", "b", "c", "c", "a"))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((x: T, us: List[U]) => f(x) :: us)

mapFun(List(1,2,3,4), (x: Int) => "a")

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x: T, n: Int) => n + 1)

lengthFun(List(1,2,3,4,7,8,8))


