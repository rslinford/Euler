package com.linfords.euler

import java.math.BigInteger

/*
Euler Project Problem 25
30 August 2002

The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
*/
object Solution025 extends App with Code025 {
  Unit025.run
  println
  val startTime = System.currentTimeMillis()
  println("Calculating...")
  println("Answer " + solve)
  println
  println("Elapsed seconds: " + ((System.currentTimeMillis() - startTime) / 1000.0))
}

trait Code025 {
  def fib: Stream[(Int, BigInteger)] = {
    def recurse(n: Int, f1: BigInteger = new BigInteger("0"), f2: BigInteger = new BigInteger("1")): Stream[(Int, BigInteger)] =
      Stream.cons((n, f2), recurse((n + 1), f2, f1 add f2))
    recurse(1)
  }

  def solve = fib.find(_._2.toString.length() == 1000).head._1
}

object Unit025 extends Code025 {
  def run = {
    fib.take(50) foreach {
      x => print(x + " ")
    }
    assert(fib(0)._2.compareTo(new BigInteger("1")) == 0)
    assert(fib(1)._2.compareTo(new BigInteger("1")) == 0)
    assert(fib(2)._2.compareTo(new BigInteger("2")) == 0)
    assert(fib(3)._2.compareTo(new BigInteger("3")) == 0)
    assert(fib(11)._2.compareTo(new BigInteger("144")) == 0)
    assert(fib.find(_._1 == 12).head._2.compareTo(new BigInteger("144")) == 0)
    assert(fib.find(_._2.toString.length() == 3).head._1 == 12)
    println("All systems go!")
  }
}