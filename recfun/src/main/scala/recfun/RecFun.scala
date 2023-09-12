package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
}

  /**
   * Exercise 2
   */
  @tailrec
  def balanceTailRec(chars: List[Char], accum:Int):Boolean = {
    if (chars.isEmpty) {
      return (accum == 0)
    }
    else{
      val head = chars.head
      val res =
      {
        if (head == '(') accum + 1
        //we can close a bracket only if it has already been opened (check of the order)
        else if (head == ')' && accum > 0) accum - 1
        else accum
      }
      //if we begin by closing a bracket we stop the recursion
      if (chars.head == ')' && accum == 0) false
      else balanceTailRec(chars.tail, res)
    }
  }
  def balance(chars: List[Char]): Boolean = {
    balanceTailRec(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
