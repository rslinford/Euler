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
  println("Answer: " + solve)
}

object Solution006_v2 extends App {
  def solve:Int = {
    val sumOfSquares = (1 to 100).map(x => x*x).sum
    val sum = (1 to 100).sum
    sum*sum - sumOfSquares
  }
  println("Answer: " + solve)
}

object CrossChecker extends App {
  println("s1(" + Solution006_v1.solve + ") s2(" + Solution006_v2.solve + ") cross check: " + (Solution006_v1.solve == Solution006_v2.solve))
}


