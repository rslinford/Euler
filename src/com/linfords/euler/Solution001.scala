package com.linfords.euler

/*
Credit goes to excellent tutorial by MadocDoyu:
  http://www.youtube.com/watch?v=nQMXmOBZPmI
*/


/*
Problem 1
05 October 2001

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*/
object Solution001 extends App {
  println("Sum: " + (1 until 1000).filter(n => n % 3 == 0 || n % 5 == 0).foldLeft(0)(_ + _));
}