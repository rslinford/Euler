package com.linfords.euler

/*
Euler Project Problem xyz

*/
object SolutionXYZ extends App with CodeXYZ {
  UnitXYZ.run
  println("Answer " + solve)
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