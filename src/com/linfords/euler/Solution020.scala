package com.linfords.euler

import java.math.BigInteger

/*
Euler Project Problem 20
21 June 2002

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
*/

object Solution020 extends App with Code020 {
  Unit020.run
  println("Answer " + solve)
}

trait Code020 {
  def factorial(n: Int): BigInteger = if (n == 1) BigInteger.valueOf(1) else BigInteger.valueOf(n) multiply factorial(n - 1)

  def solve = factorial(100).toString.toList.map(_ - '0').sum
}

object Unit020 extends Code020 {
  def run = {
    assert(factorial(1) == BigInteger.valueOf(1))
    assert(factorial(2) == BigInteger.valueOf(2))
    assert(factorial(3) == BigInteger.valueOf(6))
    assert(factorial(10) == BigInteger.valueOf(3628800))
    println("All systems go!")
  }
}