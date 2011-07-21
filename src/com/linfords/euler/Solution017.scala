package com.linfords.euler

/*
Euler Project Problem 17
17 May 2002

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then
there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of
"and" when writing out numbers is in compliance with British usage.
*/

object Solution017 extends App {
  def smallNumber(number: Int) = number match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
  }

  def tens(number: Int) = number match {
    case 2 => "twenty"
    case 3 => "thirty"
    case 4 => "forty"
    case 5 => "fifty"
    case 6 => "sixty"
    case 7 => "seventy"
    case 8 => "eighty"
    case 9 => "ninety"
  }

  def separate(s: String) = if (s.length() > 0) " " else ""

  def separateAnd(number: Int) = if (number == 0) "" else " and"

  def countAlpha(s: String) = "[a-z]".r.findAllIn(s).map(_.length()).sum

  def numberToEnglish(number: Int, builder: String = ""): String = {
    if (number > 999) numberToEnglish(number % 1000, builder + separate(builder) + smallNumber(number / 1000) + " thousand")
    else if (number > 99) numberToEnglish(number % 100, builder + separate(builder) + smallNumber(number / 100) + " hundred" + separateAnd(number % 100))
    else if (number > 19) numberToEnglish(number % 10, builder + separate(builder) + tens(number / 10))
    else if (number > 0) numberToEnglish(0, builder + separate(builder) + smallNumber(number))
    else builder
  }

  def solve: Int = {
    (1 to 1000).map(numberToEnglish(_)).map(countAlpha(_)).sum
  }

  println("Answer: " + solve)
}

object Unit0117 extends App {
  assert(Solution017.numberToEnglish(115) == "one hundred and fifteen");
  assert(Solution017.countAlpha(Solution017.numberToEnglish(115)) == 20);
  val s342 = Solution017.numberToEnglish(342)
  assert(s342 == "three hundred and forty two");
  assert(Solution017.countAlpha(s342) == 23);

  println("All systems go")
}