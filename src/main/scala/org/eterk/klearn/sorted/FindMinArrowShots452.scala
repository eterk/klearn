package org.eter.klearn
package sorted

import scala.collection.mutable.ArrayBuffer

object FindMinArrowShots452 {
  def s1(points: Array[Array[Int]]): Int = {
    var i = 0
    val pSorted = points.sortBy(i => i(0))
    var r = 1
    var center = pSorted.head
    val tail = pSorted.tail
    while (i < tail.length) {
      // 没有交集
      if (center(1) < tail(i)(0)) {
        r += 1
        center = tail(i)
      } else {
        // 有交集
        center(0) = Math.max(center(0), tail(i)(0))
        center(1) = Math.min(center(1), tail(i)(1))
      }
      i += 1
    }

    r
  }

  def apply(points: Array[Array[Int]]): Int = {

    s1(points)

  }

  def main(args: Array[String]): Unit = {


    println(apply(Array(Array(10, 16), Array(2, 8), Array(1, 6), Array(7, 12))))
  }
}
