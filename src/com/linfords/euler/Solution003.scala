package com.linfords.euler

/*
Euler Project Problem 3
02 November 2001

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600351475143 ?
*/

object Data003 {
  val theNumber = 600851475143L;
}

object Solution003 extends App {
  def solve: Long = {

    def isFactor(k:Long, n:Long) = n%k==0

    def ldf(n:Long, k:Long = 2):Long = {
      if (isFactor(k,n)) k
      else if (k*k>n) n
      else ldf(n, k+1)
    }

    def factors(x:Long):List[Long] = {
      if (x == 1L) Nil
      else {
        val p = ldf(x)
        p :: factors(x/p)
      }
    }
    factors(Data003.theNumber).last
  }

  println("Answer: " + solve)
}
