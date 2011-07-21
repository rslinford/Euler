package com.linfords.euler

import java.lang.Long

/*
Euler Project Problem 15
19 April 2002

Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?

*/

case class Point(val x: Int, val y: Int)

trait Helper015 {
  val gridSize = 10
  val startPoint = Point(0, 0)
  val endPoint = Point(gridSize, gridSize)

  val rightOffset = Point(1, 0)
  val downOffset = Point(0, 1)

  def solutionTimer(solve: () => Long): Long = {
    val startTime = System.currentTimeMillis();
    val result = solve()
    val elapsedTime = System.currentTimeMillis() - startTime;
    println("Elapsed minutes: " + elapsedTime / 1000.0 / 60.0)
    result
  }

  def showResult(tag: String, solve: () => Long) = {
    println("Answer " + tag + ": " + solutionTimer(solve))
  }
}

/* Brute force single threaded.

gridSize         routes     Minutes
========  =============   =========
      2               6
      3              20
      4              70
      5             252
      6             924
      7           3,432
      8          12,870
      9          48,620
     10         184,756     0.005
     11         705,432
     12       2,704,156
     13      10,400,600     0.119
     14      40,116,600     0.368
     15     155,117,520     1.56
     16     601,080,390     5.83
     17   2,333,606,220    22.06
     18   9,075,135,300    89.23
     20 137,846,528,820  1334.42 = 22.24 hours; more than 800,000,000,000 recursive method iterations
*/
object Solution015 extends App with Helper015 {

  var c: Long = 0

  def travel(p: Point = startPoint): Long = {
    if (p == endPoint) 1
    else if (p.x > endPoint.x || p.y > endPoint.y) 0
    else (for {v <- List(rightOffset, downOffset)} yield travel(Point(v.x + p.x, v.y + p.y))).foldLeft(new Long(0))(_ + _)
  }

  showResult("v1", () => travel())
}
