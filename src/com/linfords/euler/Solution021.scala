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
  println()
  println("Answer " + solve)
}

trait Code021 {
  def properDivisors(n: Int) = n match {
    case 0 => List()
    case 1 => List(1)
    case _ => (1 to n / 2).filter(n % _ == 0).toList
  }

  def isAmicable(a: Int) = {
    val b = properDivisors(a).sum
    if (a == b) false
    else properDivisors(b).sum == a
  }

  def amicableNumbers(n: Int = 1): Stream[Int] = if (isAmicable(n)) Stream.cons(n, amicableNumbers(n + 1)) else amicableNumbers(n + 1)

  def solve = amicableNumbers().takeWhile(_ < 10000).sum
}

object Unit021 extends Code021 {
  def run = {
    assert(amicableNumbers().takeWhile(_ < 300).toList == List(220, 284))

    assert(isAmicable(220))
    assert(isAmicable(284))
    assert(!isAmicable(1))
    assert(!isAmicable(234))

    assert(properDivisors(1) == List(1))
    assert(properDivisors(2) == List(1))
    assert(properDivisors(3) == List(1))
    assert(properDivisors(4) == List(1, 2))
    assert(properDivisors(5) == List(1))
    assert(properDivisors(6) == List(1, 2, 3))
    assert(properDivisors(220) == List(1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110))
    assert(properDivisors(284) == List(1, 2, 4, 71, 142))

    println("All systems go!")
  }
}
