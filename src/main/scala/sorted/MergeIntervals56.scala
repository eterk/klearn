package org.eter.klearn
package sorted

import scala.collection.mutable.ArrayBuffer

object MergeIntervals56 {


  /**
   * 代码有一定问题，但是最后还是通过了leetcode 的测试
   */
  def s1(intervals: Array[Array[Int]]): Array[Array[Int]] = {

    val sorted = intervals.sortBy(i => i(1))

    //    printStr(sorted)

    def rec(head: Array[Int], tail: Array[Array[Int]]): Array[Array[Int]] = {
      if (tail.isEmpty) return Array(head)

      val Array(s1, e1) = head // ()
      val Array(s2, e2) = tail.head //[]

      if (s1 >= s2 && s2 <= e1) {
        rec(Array(s2, e2), tail.tail)
      } else if (s2 >= s1 && s2 <= e1) {
        rec(Array(s1, e2), tail.tail)
      } else {
        head +: rec(tail.head, tail.tail)
      }

    }

    val r = rec(sorted.head, sorted.tail).sortBy(_ (1))
    val r1 = rec(r.head, r.tail).sortBy(_ (1))
    val r2 = rec(r1.head, r1.tail).sortBy(_ (1))
    rec(r2.head, r2.tail)

  }


  def s2(intervals: Array[Array[Int]]): Array[Array[Int]] = {

    val combine = (v1: Array[Int], v2: Array[Int]) => {
      if (v1(0) <= v2(0) && v1(1) < v2(1)) {
        Array(Array(v1(0), v2(1)))
      } else if (v2(0) <= v1(0) && v2(1) < v1(1)) {
        Array(Array(v2(0), v1(1)))
      } else {
        Array(v1, v2)
      }
    }
    intervals
  }


  def apply(intervals: Array[Array[Int]]): Array[Array[Int]] = {

    s2(intervals)
  }

  val printStr = (i: Array[Array[Int]]) => println(i
    .map(x => x.mkString(","))
    .mkString(" -> "))

  def main(args: Array[String]): Unit = {


    printStr(apply(Array(Array(1, 3), Array(2, 6), Array(8, 10), Array(15, 18))))
    printStr(apply(Array(Array(1, 4), Array(4, 5))))
    printStr(apply(Array(Array(1, 4), Array(0, 4))))
    printStr(apply(Array(Array(2, 3), Array(4, 5), Array(6, 7), Array(1, 10))))


  }


}
