package com.linfords.euler

/*
Euler Project Problem 10
08 February 2002

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*/

/*
Credit goes to Eratosthenes

http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

 1. Create a list of consecutive integers from two to n: (2, 3, 4, ..., n).
 2. Initially, let p equal 2, the first prime number.
 3. Starting from p, count up by p and cross out thus found numbers in the list (which will be 2p, 3p, 4p, etc.).
 4. Find the first number not yet crossed out after p; let p now equal this number (which is the next prime).
 5. Repeat steps 3 and 4 until p is greater than n.
 6. All the numbers in the list which are not crossed out are prime.
*/
object Solution010_v1 extends App {
  def solve: Int = {
    def nextPrime(pList: List[Int], candidate: Int = 0): Int = {
      val n = if (candidate == 0) pList.last + 1 else candidate
      for (p <- pList) {
        if (n % p == 0) return nextPrime(pList, n + 1)
      }
      n
    }
    def primeStream(pList: List[Int] = Nil): Stream[Int] = {
      val np = if (pList == Nil) 2 else nextPrime(pList)
      Stream.cons(np, primeStream(pList ::: List(np)))
    }
    primeStream().takeWhile(_ < 10).sum
  }

  println("Answer 1: " + solve)
}
