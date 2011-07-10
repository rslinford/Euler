package com.linfords.euler

import javax.xml.crypto.Data
import java.lang.Boolean
import com.sun.xml.internal.ws.developer.MemberSubmissionAddressing.Validation

/*
Euler Project Problem 4
16 November 2001

A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*/

object Data004 {
  val n1 = 100
  val n2 = 999
}

object Solution004 extends App {
  def solve: Int = {
    def isPalindromic (n:Int): Boolean = {
      val s = n.toString
      var a = 0
      var b = s.length() - 1
      while (a < b) {
        if (s.charAt(a) != s.charAt(b)) return false
        a += 1;
        b -= 1;
      }
      true
    }

    def f(x1:Int, x2:Int):Int = {
      var m = 0
      for (a <- x1 to x2)
        for (b <- a to x2) {
          val p = a*b
          if (isPalindromic(p) && p > m) {m = p}
        }
      m
    }


    f(Data004.n1, Data004.n2)
  }

  println("Answer: " + solve)
}
