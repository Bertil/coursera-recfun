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
    def pascal(c: Int, r: Int): Int =
      if(c == 0 | c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean ={
      def parse(n_open: Int, chars: List[Char] ):Int = {
        if(chars.isEmpty | n_open < 0) n_open
        else if(chars.head == '(') parse(n_open +1, chars.tail)
        else if(chars.head == ')') parse(n_open - 1, chars.tail)
        else parse(n_open,chars.tail)
      }
      parse(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0 | coins.isEmpty) 0
      else if(money < coins.sorted(Ordering.Int.reverse).head) countChange(money, coins.sorted(Ordering.Int.reverse).tail)
      else if(money == coins.sorted(Ordering.Int.reverse).head) 1 + countChange(money, coins.sorted(Ordering.Int.reverse).tail)
      else countChange(money - coins.sorted(Ordering.Int.reverse).head, coins) + countChange(money, coins.sorted(Ordering.Int.reverse).tail)
    }
  }
