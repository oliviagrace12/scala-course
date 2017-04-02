package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) return 1
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def doBalance(chars: List[Char], balanced: Int): Boolean = {
      if (balanced == -1) {
        return false
      }
      if (chars.isEmpty) {
        return balanced == 0
      }
      var balTemp = balanced
      if (chars.head == '(') {
        balTemp += 1
      }
      if (chars.head == ')') {
        balTemp -= 1
      }
      doBalance(chars.tail, balTemp)
    }

    doBalance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) {
      return 0
    }
    if (money == 0) return 1
    countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
