package com.linfords.euler

import java.lang.Boolean
import javax.management.remote.rmi._RMIConnection_Stub
import com.sun.xml.internal.ws.developer.MemberSubmissionAddressing.Validation

/*
Euler Project Problem 9
25 January 2002

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*/

object Solution009_v1 extends App {
  def solve: Int = {
    def isTriplet(t: (Int, Int, Int)): Boolean = t._1 * t._1 + t._2 * t._2 == t._3 * t._3
    def triplets: Stream[(Int, Int, Int)] = {
      (for (c <- 1 to 1000;
           b <- 1 until c;
           a <- 1 until b;
           val t = (a, b, c);
           if (isTriplet(t))) yield t).toStream
    }
    val theTriplet = triplets.filter(t => t._1 + t._2 + t._3 == 1000).take(1).foldLeft((0,0,0))((b,a) => a)
    theTriplet._1 * theTriplet._2 * theTriplet._3
  }
  println ("Answer 1: " + solve)
}
