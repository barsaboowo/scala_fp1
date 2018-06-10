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
    if (c == 0 || r == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def bal(chars: List[Char], count: Int): Boolean = chars match {
      case Nil => count == 0
      case c :: cs => if (count < 0) false
      else {
        if ('(' == c) bal(cs, count + 1)
        else if (')' == c) bal(cs, count - 1)
        else bal(cs, count)
      }
    }
    bal(chars, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else{
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }

  }
}
