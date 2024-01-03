package org.eter.klearn

import scala.annotation.tailrec

object Fibonacci {
  def dynamicProgramRoll(n: Int): Int = {
    val res = new Array[Int](3)
    res(0) = 0
    res(1) = 1
    var i = 0
    while (i < n) {
      res(2) = res(0) + res(1)
      res(0) = res(1)
      res(1) = res(2)
      i += 1
    }
    res(0)
  }


  def rec1(n: Int): Int = {
    var i = n

    @tailrec
    def rec(n: Int, m: Int): Int = {
      if (i == 0) return n
      i -= 1
      rec(m, m + n)
    }

    rec(0, 1)
  }

  def apply(n: Int): Int = {
    rec1(n)
  }

  def main(args: Array[String]): Unit = {

    println((0 to 10).map(rec1).mkString(","))
    println((0 to 10).map(dynamicProgramRoll).mkString(","))

  }

}
