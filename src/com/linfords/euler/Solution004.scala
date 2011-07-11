package com.linfords.euler

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

object Solution004_v1 extends App {
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

  println("Answer 1: " + solve)
}

object Solution004_v2 extends App {
  def solve: Int = {
    def isPal (n:Int):Boolean = isPalindromic(n.toString().toList)
    def isPalindromic (c:List[Char]):Boolean = {
      if (c.length < 2) return true
      if (c.head != c.last)
        return false
      else
        isPalindromic(c.slice(1,c.length-1))
    }

    def uniqueProducts(r:Range):List[Int] = {
      if (r.head < r.last)
        r.map(r.head * _).toList ::: uniqueProducts(r.head+1 to r.last)
      else
        Nil
    }

    uniqueProducts(Data004.n1 to Data004.n2).filter(isPal(_)).max
  }

  println("Answer 2: " + solve)
}

/*
A conscise solution from oscarp

http://projecteuler.net/index.php?section=forum&id=4&page=12

 */
object Solution004_v3 extends App {
  def solve:Int = {
    (for(x <- 100 to 999;
         y <- x to 999;
         val p = x * y
         if (p.toString.reverse.mkString == p.toString )) yield p).foldLeft(0)((a,b) => if (b > a) b else a)
  }

  println("Answer 3: " + solve)
}

object CrossCheck004 extends App {
  val s = List(Solution004_v1.solve, Solution004_v2.solve, Solution004_v3.solve)
  println(s + " cross check: " + s.foldLeft(true)((b,a) => b && a==s.head))
}
