package com.linfords.euler

/*
Credit goes to the excellent tutorial by MadocDoyu:

  http://www.youtube.com/watch?v=01Fy-Wf2zzk
*/


/*
Euler Project Problem 6
14 December 2001

The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*/

object Solution006_v1 extends App {
  def solve:Int = {
    var sumOfSquares = 0;
    var sum = 0;
    for (i <- (1 to 100)) {
      sumOfSquares += i*i
      sum += i
    }
    sum*sum - sumOfSquares
  }
  println("Answer 1: " + solve)
}

object Solution006_v2 extends App {
  def solve:Int = {
    val sumOfSquares = (1 to 100).map(x => x*x).sum
    val sum = (1 to 100).sum
    sum*sum - sumOfSquares
  }
  println("Answer 2: " + solve)
}

object CrossChecker006 extends App {
  val s1 = Solution006_v1.solve
  val s2 = Solution006_v2.solve
  println("s1(" + s1 + ") s2(" + s2 + ") cross check: " + (s1 == s2))
}


