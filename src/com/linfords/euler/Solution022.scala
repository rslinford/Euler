package com.linfords.euler

import io.Source

/*
Euler Project Problem 22
19 July 2002

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing
over five-thousand first names, begin by sorting it into alphabetical order. Then
working out the alphabetical value for each name, multiply this value by its alphabetical
position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
*/
object Solution022 extends App with Code022 with Data022 {
  Unit022.run
  println
  val startTime = System.currentTimeMillis()
  println("Calculating...")
  println("Answer " + solve(sortedNames))
  println
  println("Elapsed seconds: " + ((System.currentTimeMillis() - startTime) / 1000.0))
}

trait Code022 {
  def alphaValue(s: String) = s.map(_ - 'A' + 1).sum

  def nameScore(s: String, ls: List[String]) = alphaValue(s) * (ls.indexOf(s) + 1)

  def solve(ls: List[String]) = ls.map(nameScore(_, ls)).sum
}

trait Data022 {
  val names = """\w+""".r.findAllIn(Source.fromFile("/home/user/IdeaProjects/Euler/src/com/linfords/euler/names022.txt").mkString).toList
  val sortedNames = names.sorted
}

object Unit022 extends Code022 with Data022 {
  def run = {
    // Verify reading and parsing of file
    assert(names(4472) == "COLIN")
    assert(names(0) == "MARY")
    assert(names(names.size - 1) == "ALONSO")
    assert(names.size == 5163)

    assert(sortedNames(937) == "COLIN")
    assert(alphaValue("COLIN") == 53)
    assert(nameScore("COLIN", sortedNames) == 49714)

    println("All systems go!")
  }
}
