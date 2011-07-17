package com.linfords.euler

/*
Euler Project Problem 14
05 April 2002

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*/


object Solution014 extends App {
  def sequence(n: Long): Stream[Long] =
    if (n == 1) {
      Stream(n)
    }
    else {
      val n2 = if (n % 2 == 0) n / 2 else 3 * n + 1;
      Stream.cons(n, sequence(n2))
    }

  def solve: Int = {
    (1 to 1000000).map((n: Int) => (n, sequence(n).size)).foldLeft(0, 0)((b, a) => if (a._2 > b._2) a else b)._1
  }

  println("Answer: " + solve)
}

object Unit014 extends App {
  assert(Solution014.sequence(1) == List(1))
  assert(Solution014.sequence(2) == List(2, 1))
  assert(Solution014.sequence(13) == List(13, 40, 20, 10, 5, 16, 8, 4, 2, 1))
  println("All systems go!")
}