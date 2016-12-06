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
      def pascalIter(c: Int, r: Int): Int = 
        if (r == 0 || c == 0 || c == r) 1 
        else pascalIter(c-1, r-1) + pascalIter(c, r-1)
      
      pascalIter(c, r)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIter(count: Int, chars: List[Char], last: Char): Boolean = 
        if (chars.isEmpty) {
          if (last==')')
            count == 0
          else false
        }
        else if (chars.head=='(') {
          balanceIter(count+1, chars.tail, '(')
        }
        else if (chars.head==')') {
          balanceIter(count-1, chars.tail, ')')
        }
        else
          balanceIter(count, chars.tail, last)
      
      if (chars.isEmpty || (!chars.contains('(') && !chars.contains(')')))
        true
      else
    	balanceIter(0, chars, chars.head)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = 
        // we have change!
        if (money==0) 1
        // we don't have change!
        else if (coins.isEmpty || money < 0) 0
        // count the rest of coins, count the total - first coin using the all the coins, and add them together
        else 
          countChange(money, coins.tail) + countChange(money-coins.head, coins)
  }
