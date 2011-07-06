package com.linfords.euler

/*
Euler Project Problem 8
11 January 2002

Find the greatest product of five consecutive digits in the 1000-digit number.
*/

object Data008 {
  val blockOfDigits = """
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
"""

  def bigNumber = {
    BigInt(blockOfDigits.replace("\n",""))
  }

  val digitsPerProduct = 5;
}
/*
My attempt at a declarative approach
 */
object Solution008_v1 extends App {
  def solve:Int = {
    def multiplyDigits(s:String, offset:Int, n:Int):Int = {
      if (n < 1) 1
      else s.charAt(offset).asDigit * multiplyDigits(s, offset+1, n-1)
    }

    def maxProduct(s:String, digitQty:Int, offset:Int = 0, product:Int = 0):Int = {
      if (offset > s.length()-digitQty) return product
      val p = multiplyDigits(s, offset, digitQty)
      maxProduct(s, digitQty, offset+1, p.max(product))
    }
    maxProduct(Data008.bigNumber.toString(), Data008.digitsPerProduct)
  }
  println("Answer 1: " + solve)
}

/*
Credit goes to the excellent tutorial by MadocDoyu:

  http://www.youtube.com/watch?v=SpJmWy8Rfwg
*/

object Solution008_v2 extends App {
  def solve:Int = {
    def greatestProduct(digitList:List[Int]):Int = {
      digitList match {
        case List(d1, d2, d3, d4, d5, _*) => (d1*d2*d3*d4*d5).max(greatestProduct(digitList.tail))
        case _ => 0
      }
    }
    greatestProduct(Data008.bigNumber.toString().map(_.asDigit).toList)
  }
  println("Answer 2: " + solve)
}

object CrossChecker008 extends App {
  val s1 = Solution008_v1.solve
  val s2 = Solution008_v2.solve
  println("s1(" + s1+ ") s2(" + s2 + ") cross check: " + (s1 == s2))
}
