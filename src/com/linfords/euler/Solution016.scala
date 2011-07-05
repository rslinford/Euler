package com.linfords.euler

import javax.management.remote.rmi._RMIConnection_Stub
import java.math.BigInteger
import java.lang.Math

/*
Euler Project Problem 16
03 May 2002

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
*/

/*
My Java infested solution
 */
object Solution016_v1 extends App {
  def solve:Int = {
    val ba = BigInteger.valueOf(2).pow(1000).toString.getBytes;
    println(ba)
    var sum = 0;
    for (b <- ba) {
      sum += b-48
    }
    sum
  }
  println("Answer 1: " + solve)
}

/*
Credit goes to the excellent tutorial by MadocDoyu:

  http://www.youtube.com/watch?v=j6ztysi9Xww
*/

object Solution016_v2 extends App {
  def solve:Int = {
    BigInt(2).pow(1000).toString().map(_.asDigit).sum;
  }
  println("Answer 2: " + solve)
}

object CrossChecker016 extends App {
  val s1 = Solution016_v1.solve
  val s2 = Solution016_v2.solve
  println("s1(" + s1+ ") s2(" + s2 + ") cross check: " + (s1 == s2))
}
