package org.eterk.klearn.untyped

import scala.collection.mutable

object ChalkReplacer1894 {

  import scala.annotation.tailrec

  @tailrec
  def rec(chalk: Array[Int], k: Int, index: Int): Int = {
    val left = k - chalk(index)
    if (left < 0) return index

    val newI = if (index < chalk.length - 1) {
      index + 1
    } else {
      0
    }
    rec(chalk, left, newI)
  }


  def reduceArray(chalk: Array[Int]): Array[Int] = {
    val r = mutable.Buffer[Int]()
    var current = 0
    var tmp = 0
    for (i <- chalk) {
      tmp = i + current
      if (i + current <= Int.MaxValue) {
        current = tmp
      } else {
        r.append(i)
        current = i
      }
    }
    r.toArray
  }

  def chalkReplacer1(chalk: Array[Int], k: Int): Int = {

    rec(chalk, k, 0)

  }


  def chalkReplacer(chalk: Array[Int], k: Int): Int = {

    rec(chalk, k % chalk.sum, 0)

  }

  def apply(chalk: Array[Int], k: Int): Int = {

    chalkReplacer(chalk, k)

  }

  def req(l: Boolean) = {
    require(
      l

    )
  }

  def main(args: Array[String]): Unit = {
    req(apply(Array(5, 1, 5), 22) == 0)
    req(apply(Array(3, 4, 1, 2), 25) == 1)

    req(apply(Array(30, 76, 46, 74, 34, 12, 1, 82, 25, 28, 63, 29, 60, 76, 98, 20, 40, 32, 76, 26, 71), 346237330) == 19)
    apply(Array(30, 76, 46, 74, 34, 12, 1, 82, 25, 28, 63, 29, 60, 76, 98, 20, 40, 32, 76, 26, 71), 539095482) == 19


  }

}
