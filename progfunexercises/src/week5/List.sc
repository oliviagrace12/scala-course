import week4.{Cons, Nil}

object List {
  def List() = () => new Nil
  def List[T](x: T) = (x: T) => new Cons(x, new Nil)
  def List[T](x: T, y: T) =
    (x: T, y: T) => new Cons(x, new Cons(y, new Nil))


  List(1, 2)

}