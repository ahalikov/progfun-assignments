package week1

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
    if (r < 2)
      1
    else {
      if (c == 0 || c == r)
        1
      else
        pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def stack(depth: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) {
        depth == 0
      } else {
        val c:Char = chars.head
        if (c == '(')
          stack(depth+1, chars.tail)
        else if (c == ')') {
          if (depth == 0)
            false
          else
            stack(depth - 1, chars.tail)
        }
        else
          stack(depth, chars.tail)
      }

    stack(0, chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      0
    else {
      def countWays(money: Int, coins: List[Int], count: Int): Int =
        if (money < 0)
          count
        else if (coins.isEmpty) {
          if (money == 0) count + 1 else count
        }
        else
          countWays(money, coins.tail, count) +
            countWays(money - coins.head, coins, count)

      countWays(money, coins, 0)
    }
  }
}