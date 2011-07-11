package com.linfords.euler

/*
Euler Project Problem 5
30 November 2001

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*/

object Solution005_v1 extends App {
  def solve: Int = {
    def isDivisibleByAll(r: Range, n: Int) = r.foldLeft(true)((b, a) => b && (n % a == 0))
    def findNumber(r: Range, n: Int): Int = if (isDivisibleByAll(r, n)) n else findNumber(r, n + r.last)


    val r = 1 to 20
    findNumber(r, r.last)
  }

  println("Answer 1: " + solve)
}
