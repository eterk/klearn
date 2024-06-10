package org.eterk.klearn

import org.eterk.klearn.untyped._

object Test20240608 {

  def do20240608() = {
    HIndex274.stdTest()
      .foreach(println)
    ReverseWordsInString.stdTest()
      .foreach(println)
    IntegerToRoman.stdTest()
      .foreach(println)
  }

  def do20240609() = {
    ValidPalindrome125.stdTest()
      .foreach(println)
    Subsequence392.stdTest()
      .foreach(println)
    ThreeSum15.stdTest()
      .foreach(println)
    TwoSum167.stdTest()
      .map(x => x.copy(_3 = x._3.toSeq))
      .foreach(println)
    ValidSudoku36.stdTest()
      .foreach(println)
    SetMatrixZeroes73.stdTest()
      .map(x => x.copy(_3 = x._3.toSeq.map(_.toSeq)))
      .foreach(println)
    CanConstruct383.stdTest()
      .foreach(println)

    IsomorphicString205.stdTest()
      .foreach(println)
  }

  def do20240610(): Unit = {
    LongestConsecutiveSeq128
      .stdTest()
      .foreach(println)
    TwoSum1
      .stdTest()
      .map(x => x.copy(_3 = x._3.toSeq))
      .foreach(println)
  }

  def main(args: Array[String]): Unit = {
    GroupAnagrams49
      .stdTest()
      .map(x => x.copy(_3 = x._3.toSeq))
      .foreach(println)
  }

}