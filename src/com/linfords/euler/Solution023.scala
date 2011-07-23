package com.linfords.euler

import annotation.tailrec

/*
Euler Project Problem 23
02 August 2002

A perfect number is a number for which the sum of its proper divisors is exactly equal
to the number. For example, the sum of the proper divisors of 28 would be
1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it
is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number
that can be written as the sum of two abundant numbers is 24. By mathematical analysis,
it can be shown that all integers greater than 28123 can be written as the sum of two
abundant numbers. However, this upper limit cannot be reduced any further by analysis
even though it is known that the greatest number that cannot be expressed as the sum
of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two
abundant numbers.
*/
object Solution023 extends App with Code023 {
  Unit023.run
  println
  val startTime = System.currentTimeMillis()
  println("Calculating...")
  println("Answer " + solve)
  println
  println("Elapsed seconds: " + ((System.currentTimeMillis() - startTime) / 1000.0))
}

trait Code023 {
  val provenCutoff = 28123
  val abundantList = abundantNumbers().takeWhile(_ < provenCutoff).toList

  def properDivisors(n: Int) = n match {
    case 0 => List()
    case 1 => List(1)
    case _ => (1 to n / 2).filter(n % _ == 0).toList
  }

  def abundantNumbers(n: Int = 1): Stream[Int] = {
    if (properDivisors(n).sum > n) Stream.cons(n, abundantNumbers(n + 1))
    else abundantNumbers(n + 1)
  }

  def crossAdd(a: List[Int]): List[Int] = {
    @tailrec
    def recurse(a1: List[Int], b1: List[Int], acum: List[Int]): List[Int] = {
      val (nextA, nextB) = if (a1.tail == Nil) (a, b1.tail) else (a1.tail, b1)
      if (nextB == Nil) a1.head + b1.head :: acum
      else recurse(nextA, nextB, a1.head + b1.head :: acum)
    }
    recurse(a, a, Nil)
  }

  def solve = {
    val abundantSums = crossAdd(abundantList).toSet
    (1 until provenCutoff).filter(!abundantSums.contains(_)).sum
  }
}

object Unit023 extends Code023 {
  def run = {
    assert(abundantNumbers()(0) == 12)
    assert(abundantList.last == provenCutoff - 1)

    val ls = List(2, 7, 34)
    val ca = crossAdd(ls)
    assert(ca(0) == 68)
    assert(ca(1) == 41)
    assert(ca(2) == 36)
    assert(ca(3) == 41)
    assert(ca(4) == 14)
    assert(ca(5) == 9)
    assert(ca(6) == 36)
    assert(ca(7) == 9)
    assert(ca(8) == 4)

    println("All systems go!")
  }
}