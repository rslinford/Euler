package com.linfords.euler

/*
Euler Project Problem 21
05 July 2002

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which
divide evenly into n).

If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each
of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
*/

object Solution021 extends App with Code021 {
  Unit021.run
  println("Answer " + solve)
}

trait Code021 {
  def solve = 1234
}

object Unit021 extends Code021 {
  def run = {
    assert(1 == 1)
    println("All systems go!")
  }
}
