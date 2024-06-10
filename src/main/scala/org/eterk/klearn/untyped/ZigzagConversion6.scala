package org.eterk.klearn.untyped

import scala.collection.mutable

object ZigzagConversion6 {

  def apply(str: String, rows: Int): String = {
    s1(str, rows)
  }

  /**
   * 做了53分钟
   * 8:26->9:19
   * 提交了两次
   */
  def s1(s: String, numRows: Int): String = {
    val height = if (s.length < numRows) {
      s.length
    } else {
      numRows
    }
    var h = 0
    val container = mutable.Buffer.fill(height)(mutable.Buffer.empty[Char])
    val iter = s.iterator
    var alpha = 1
    while (iter.hasNext) {
      container(h) = container(h).append(iter.next())
      h match {
        case any if height == 1 =>
          alpha = 0
        case 0 =>
          alpha = 1
        case max if max == height - 1 =>
          alpha = -1
        case _ =>
      }

      h = h + alpha
    }


    container.map(_.mkString("")).mkString("")
  }

  /**
   * s1 代码的优化版
   */
  def s2(s: String, numRows: Int): String = {
    if (numRows == 1) return s

    val height = Math.min(s.length, numRows)
    var h = 0

    val container = mutable.Buffer.fill(height)(mutable.Buffer.empty[Char])

    val iter = s.iterator

    var alpha = 1

    while (iter.hasNext) {
      container(h) = container(h).append(iter.next())
      h match {
        case 0 =>
          alpha = 1
        case max if max == height - 1 =>
          alpha = -1
        case _ =>
      }
      h = h + alpha
    }


    container.map(_.mkString("")).mkString("")
  }

  /**
   * AI 优化后
   */
  def s3(s: String, numRows: Int): String = {
    if (numRows == 1) return s

    val height = Math.min(s.length, numRows)
    var h = 0

    val container = Array.fill(height)(new StringBuilder)

    var alpha = 1

    for (c <- s) {
      container(h).append(c)
      h match {
        case 0 =>
          alpha = 1
        case max if max == height - 1 =>
          alpha = -1
        case _ =>
      }
      h = h + alpha
    }

    container.mkString("")
  }


  def main(args: Array[String]): Unit = {

    test(s2)

  }

  def test(f: (String, Int) => String) = {
    require(f("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR")
    require(f("PAYPALISHIRING", 4) == "PINALSIGYAHRPI")
    require(f("A", 1) == "A")
    require(f("AB", 1) == "AB")
  }


}
