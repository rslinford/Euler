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

/*
Credit goes to Pavel Fatin
   http://pavelfatin.com/scala-for-project-euler/

Wish I understood how this works. Would need to study Discrete Logarithms
   http://en.wikipedia.org/wiki/Discrete_logarithm

Also helpful, Group Mathmatics
   http://en.wikipedia.org/wiki/Group_%28mathematics%29

I learned that Discrete Logarithms and Integer Factorization share properties that make them
useful in cryptography.
 */
trait Code026 {
  val ps = (2 until 1000).map(i => (1 to 2000)
    .find(BigInt(10).modPow(_, i) == 1))

  def solve = 2 + ps.indexOf(Some(ps.flatten.max))
}

object Unit026 extends Code026 {
  def run = {
    println("All systems go!")
  }
}
