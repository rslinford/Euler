package com.linfords.euler

import java.util.{Date, Calendar}

/*
Euler Project Problem 19
14 June 2002

You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*/

object Solution019 extends App {

  def isSundayTheFirst(d: Date) = {
    val cal = Calendar.getInstance()
    cal.setTime(d);
    val r = (cal.get(Calendar.DAY_OF_WEEK) == 1) && (cal.get(Calendar.DAY_OF_MONTH) == 1)
    if (r) println(d)
    r
  }

  def dayStream(cal: Calendar): Stream[Date] = {
    val date = cal.getTime
    cal.add(Calendar.DAY_OF_MONTH, 1)
    Stream.cons(date, dayStream(cal))
  }

  def solve: Int = {
    val startCal = Calendar.getInstance()
    startCal.set(1901, 0, 1, 6, 30)
    val endCal = Calendar.getInstance()
    endCal.set(2000, 11, 31, 8, 30)
    val endTime = endCal.getTime.getTime
    dayStream(startCal).takeWhile(_.getTime < endTime).count(isSundayTheFirst(_))
  }

  println("Answer: " + solve)
}