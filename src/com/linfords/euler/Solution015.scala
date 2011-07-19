package com.linfords.euler

import java.lang.Long

/*
Euler Project Problem 15
19 April 2002

Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?

*/

/* Brute force single threaded. Estimated time to completion: ~30 hours */
object Solution015_v1 extends App {

  case class Point(val x: Int, val y: Int)

  val startPoint = Point(0, 0)
  val gridSize = 20
  val endPoint = Point(gridSize, gridSize)
  var c: Long = 0

  def travel(p: Point, route: List[Point] = Nil): Long = {
    c += 1
    val r = p :: route

    if (p == endPoint) {
      if (c % 1000000000 < 20) println(c + "] " + r.reverse);
      1
    }
    else if (p.x > endPoint.x || p.y > endPoint.y) 0
    else (for {v <- List(Point(1, 0), Point(0, 1))} yield travel(Point(v.x + p.x, v.y + p.y), p :: route)).foldLeft(new Long(0))(_ + _)
  }

  def solve: Long = {
    val startTime = System.currentTimeMillis();
    val result = travel(startPoint)
    val elapsedTime = System.currentTimeMillis() - startTime;
    println("Elapsed minutes: " + elapsedTime / 1000 / 60.0)
    result
  }

  println("Answer: " + solve)
}