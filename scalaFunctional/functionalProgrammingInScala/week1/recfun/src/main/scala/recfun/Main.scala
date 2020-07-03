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
      if((c == 0) || (r == c)) {
         1
      }
      else {
        val priorRow = r - 1
        val priorCol = c - 1
        pascal(priorCol, priorRow) + pascal(c, priorRow)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def sawOpen(chars: List[Char], openCount : Int): Boolean = {
        if(openCount < 0)
          false
        else if (chars.isEmpty) {
          openCount == 0
        }
        else {
          val currChar = chars.head

          if(currChar == '(')
            sawOpen(chars.tail, openCount + 1)
          else if(currChar == ')')
            sawOpen(chars.tail, openCount - 1)
          else
            sawOpen(chars.tail, openCount)
        }
      }

      sawOpen(chars, 0)
    }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if ((money < 0) || coins.isEmpty)  0
      else countChange(money - coins.head, coins) +  countChange(money, coins.tail)
  }
}
