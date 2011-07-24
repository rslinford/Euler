package com.linfords.euler

/*
Euler Project Problem 24
16 August 2002

A permutation is an ordered arrangement of objects. For example, 3124 is one possible
permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically
or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1
and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*/
object Solution024 extends App with Code024 {
  Unit024.run
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

A one liner... amazing.
 */
trait Code024 {
def ps(s: String): Seq[String] = if(s.size == 1) Seq(s) else s.flatMap(c => ps(s.filterNot(_ == c)).map(c +))

  def solve = {
    ps("0123456789")(999999)
  }
}

object Unit024 extends Code024 {
  def run = {
    assert(ps("0123")(0) == "0123")
    assert(ps("0123")(23) == "3210")
    println("All systems go!")
  }
}
