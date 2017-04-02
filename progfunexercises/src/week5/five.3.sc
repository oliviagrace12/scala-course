def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
   
  case List(x) => List()
   
  case y :: ys => y :: init(ys)  
}   

init(List(1, 2, 3))   

def removeAt[T](n: Int, xs: List[T]) = {
      (xs take n) ::: (xs drop (n + 1))
   
}   

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)  //def flatten(xs: List[Any]): List[Any] = xs match { //  case List() => xs //  case List(x) => List(x) //  case List(List(x)) => List() ::: flatten(List(x)) //} // //flatten(List(List(1, 1), 2, List(3, List(5, 8))))  def mergesort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {   val n = xs.length / 2   if (n == 0) xs   else {     def merge(xs: List[T], ys: List[T]): List[T] =       (xs, ys) match {         case (Nil, ys) => ys         case (xs, Nil) => xs         case (x :: xs1, y :: ys1) =>           if (lt(x, y)) x :: merge(xs1, ys)           else y :: merge(xs, ys1)       }     val (fst, snd) = xs splitAt n     merge(mergesort(fst)(lt), mergesort(snd)(lt))   } }  //mergesort(List[Int](12,-8,3,7,4,2,4,-6))((x, y) => x < y)
