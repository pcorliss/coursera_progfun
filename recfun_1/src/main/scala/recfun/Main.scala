package recfun
import common._

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

//  1
//  1 1
//  1 2 1     [r 2 c 1] == [r 1 c 0] + [r 1 c 1]
//  1 3 3  1  [r 3 c 1] == [r 2 c 0] + [r 2 c 1]
//  1 4 6  4  1
//  1 5 10 10 5 1
//  1 6 15 20 15 6 1
//  1 7

//  foo(xs.head, xs.tail)

  def pascal(c: Int, r: Int): Int = {
    if (c < 0) return 0
    if (r < 0) return 0

    if (c == 0 || c == r) return 1

    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
//  chars.isEmpty: Boolean returns whether a list is empty
//  chars.head: Char returns the first element of the list
//  chars.tail: List[Char] returns the list without the first element
  def balance(chars: List[Char]): Boolean = {
    def balancer(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty && open == 0) return true
      if (chars.isEmpty) return false
      if (open < 0) return false
      if (chars.head == '(') return balancer(chars.tail, open + 1)
      if (chars.head == ')') return balancer(chars.tail, open - 1)
      balancer(chars.tail, open)
    }

    balancer(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) return 0
    if (money - coins.head > 0) return countChange(money - coins.head, coins) + countChange(money, coins.tail)
    if (money - coins.head == 0) return 1 + countChange(money, coins.tail)
    countChange(money, coins.tail)
  }
}
