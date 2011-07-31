package com.linfords.euler

/*
Euler Project Problem 27
27 September 2002

Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However,
when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 +
41 is clearly divisible by 41.

Using computers, the incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for
the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum
number of primes for consecutive values of n, starting with n = 0.
*/
object Solution027 extends App with Code027 {
  Unit027.run
  println
  val startTime = System.currentTimeMillis()
  println("Calculating...")
  println("Answer " + solve)
  println
  println("Elapsed seconds: " + ((System.currentTimeMillis() - startTime) / 1000.0))
}

trait Code027 {

  /*
  Credit for isPrime goes to Phil Gold:
     http://aperiodic.net/phil/scala/s-99/p31.scala
   */
  def isPrime(i: Int): Boolean = {
    if (i <= 1)
      false
    else if (i == 2)
      true
    else
      !(2 to (i - 1)).exists(x => i % x == 0)
  }

  def quadratic(n: Int, a: Int, b: Int) = n * n + a * n + b

  def consecutivePrimes(a: Int, b: Int, n: Int = 0): Int = if (!isPrime(quadratic(n, a, b))) n else consecutivePrimes(a, b, n + 1)

  def solve = (-999 to 999).map(a => (-999 to 999).map(b => (consecutivePrimes(a, b), a * b)).
    foldLeft((0, 0))((d, c) => if (c._1 > d._1) c else d)).foldLeft(0,0)((f, e) => if (e._1 > f._1) e else f)._2
}

object Unit027 extends Code027 {
  def run = {
    assert(isPrime(3))
    assert(isPrime(101))
    assert(!isPrime(55))

    assert(consecutivePrimes(1, 41) == 40)
    assert(consecutivePrimes(-79, 1601) == 80)


    println("All systems go!")
  }
}
