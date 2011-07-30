package com.linfords.euler

/*
Euler Project Problem 26
13 September 2002

A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with
denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit
recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
*/
object Solution026 extends App with Code026 {
  Unit026.run
  println
  val startTime = System.currentTimeMillis()
  println("Calculating...")
  println("Answer " + solve)
  println
  println("Elapsed seconds: " + ((System.currentTimeMillis() - startTime) / 1000.0))
}

trait Code026 {
  def decimalPart(denominator: Int, numerator: Int = 1): Stream[Int] = {
    val numeratorShifted = numerator * 10
    val digit = numeratorShifted / denominator
    val remainder = numeratorShifted % denominator
    if (remainder == 0) Stream(digit)
    else Stream.cons(digit, decimalPart(denominator, remainder))
  }

  (2 to 100).map(d => (d, decimalPart(d))).foreach(t => {
    print(t._1 + " ")
    t._2.take(50) foreach {
      print
    };
    println()
  })

  def solve = 1234
}

object Unit026 extends Code026 {
  def run = {
    assert(decimalPart(2) == Seq(5))
    assert(decimalPart(3).take(5) == Seq(3, 3, 3, 3, 3))
    assert(decimalPart(4) == Seq(2, 5))
    assert(decimalPart(7).take(12) == Seq(1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7))
    assert(decimalPart(10) == Seq(1))
    println("All systems go!")
  }
}
