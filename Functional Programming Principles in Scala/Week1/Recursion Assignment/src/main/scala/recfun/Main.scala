package recfun

import util.control.Breaks._

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

      //Iterative Approach
      /*val a = Array.ofDim[Int](r+1,r+1)
      for (line <- 0 to r) {
        for (i <- 0 to line) {
          if (line == i || i == 0) {
            a(line)(i) = 1
          }
          else {
            a(line)(i) = a(line-1)(i-1) + a(line-1)(i)
          }
        }
      }

      a(r)(c)*/

      // Recursive Approach

      if (c == r || c == 0) 1
      else pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      var open = 0
      var ans = true
      breakable {
        for (i <- 0 until chars.length) {
          if (chars(i) == '(') {
            open += 1
          }
          else if (chars(i) == ')') {
            if (open > 0) {
              open -= 1
            }
            else {
              ans = false
              break
            }
          }
        }
      }

      if (open > 0) ans = false
      ans
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val combinations :Array[Int] = new Array[Int](money+1)
      combinations(0) = 1
      for (coin <- coins) {
        for (i <- coin to money) {
          if (i >= coin) {
            combinations(i) += combinations(i-coin)
          }
        }
      }

      combinations(money)
    }
  }
