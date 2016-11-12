package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 13) {
      for (i <- row to 13) print(" ")
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r)
      0
    else if (c == 0)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def hasBalance(chars: List[Char], balance: Int): Boolean = {
      if (chars.isEmpty)
        balance == 0
      else if (balance < 0)
        false
      else if (chars.head == '(')
        hasBalance(chars.tail, balance + 1)
      else if (chars.head == ')')
        hasBalance(chars.tail, balance - 1)
      else
        hasBalance(chars.tail, balance)
    }
    hasBalance(chars, 0)
  }

  def balance_(chars: List[Char]): Boolean = {

    def is_open(c: Char) = c == '('
    def is_close(c: Char) = c == ')'

    def expectEnd(chars: List[Char]): Boolean = {
      if (chars.isEmpty)
        true
      else if (is_close(chars.head))
        false
      else
        expectEnd(
          if (is_open(chars.head))
            tailAfterClose(chars.tail)
          else
            chars.tail
        )
    }

    def tailAfterClose(chars: List[Char]): List[Char] = {
      if (chars.isEmpty)
        throw new IllegalStateException()
      else if (is_close(chars.head))
        chars.tail
      else
        tailAfterClose(
          if (is_open(chars.head))
            tailAfterClose(chars.tail)
          else
            chars.tail
        )
    }

    expectEnd(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
