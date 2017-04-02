def tailSum(f: Int => Int)(a: Int, b: Int): Int = {
  def doSum(a: Int, acc: Int): Int = {
    if (a > b) acc
    else doSum(a + 1, a + acc)
  }
  doSum(a, 0)
}

def sum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)

def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

product(x => x)(1, 3)

def factorial(x: Int): Int = product(x => x)(1, x)

factorial(4)

def operation(f: Int => Int)(a: Int, b: Int, end: Int): Int = {
  if (a > b) end
  else f(a) * product(f)(a + 1, b)
}

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, end: Int)(a: Int, b:Int): Int = {
  if (a > b) end
  else combine(f(a), mapReduce(f, combine, end)(a + 1, b))
}



