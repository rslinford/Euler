package com.linfords.euler

import java.lang.{Boolean, Math}

/*
Euler Project Problem 7
28 December 2001

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10001st prime number?
*/

object Solution007_v1 extends App {
  def solve: Int = {
    def isPrime(n: Int): Boolean = {
      def test(r: Range): Boolean = {
        if (r.isEmpty) return true
        if (n % r.head == 0) false else test(r.tail)
      }
      val sqrt = Math.sqrt(n).floor.toInt
      test(2 to sqrt)
    }

    def findPrime(targetPrime: Int, primeCounter: Int = 0, candidate: Int = 2): Int = {
      val pc = if (isPrime(candidate)) primeCounter + 1 else primeCounter
      if (targetPrime == pc) return candidate else findPrime(targetPrime, pc, candidate + 1)
    }

    findPrime(10001);
  }

  println("Answer 1: " + solve)
}
