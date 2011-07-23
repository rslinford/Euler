package com.linfords.euler

/*
Euler Project Problem xyz

*/
object SolutionXYZ extends App with CodeXYZ {
  UnitXYZ.run
  println
  val startTime = System.currentTimeMillis()
  println("Calculating...")
  println("Answer " + solve)
  println
  println("Elapsed seconds: " + ((System.currentTimeMillis() - startTime) / 1000.0))
}

trait CodeXYZ {
  def solve = 1234
}

object UnitXYZ extends CodeXYZ {
  def run = {
    assert(1 == 1)
    println("All systems go!")
  }
}