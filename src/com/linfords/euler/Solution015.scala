package com.linfords.euler

import com.linfords.euler.Solution011.Point

/*
Euler Project Problem 15
19 April 2002

Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.

How many routeCount are there through a 20×20 grid?

*/

object Solution015 extends App {

  case class Point(val x: Int, val y: Int)

  val startPoint = Point(0, 0)
  val endPoint = Point(20, 20)

  def travel(p: Point, currentRoute: List[Point] = Nil): Int = {
    if (p == endPoint) 1
    else if /* out of bounds */ (p.x < startPoint.x || p.y < startPoint.y || p.x > endPoint.x || p.y > endPoint.y) 0
    else if /* been there */ (currentRoute.contains(p)) 0
    else if /* back track */ (currentRoute != Nil && (p.x < currentRoute.head.x || p.y < currentRoute.head.y)) 0
    else (for {v <- List(Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))} yield travel(Point(v.x + p.x, v.y + p.y), p :: currentRoute)).sum
  }

  def solve: Int = {
    travel(startPoint)
  }

  println("Answer: " + solve)
}