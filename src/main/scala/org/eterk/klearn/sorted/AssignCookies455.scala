package org.eter.klearn
package sorted

object AssignCookies455 {
  def apply(g: Array[Int], s: Array[Int]): Int = {

    s1(g, s)
  }

  def s1(g1: Array[Int], s1: Array[Int]): Int = {
    if (s1.isEmpty) return 0
    val g = g1.sorted
    val s = s1.max
    var i = g.length - 1

    while (i >= 0) {
      if (g(i) <= s) {
        return Math.min(i + 1, s1.length)
      }

      i -= 1
    }

    i
  }

  def s2(g1: Array[Int], s1: Array[Int]): Int = {

    val g = g1.sorted
    val s = s1.sorted
    var i = 0
    var j = 0
    var r = 0
    while (i < g.length && j < s.length) {

      if (g(i) <= s(j)) {
        r += 1
        j += 1
        i += 1
      } else {
        j += 1
      }

    }
    r
  }

  def main(args: Array[String]): Unit = {

    println(s2(Array(1, 2, 3), Array(1, 1)))
    println(s2(Array(10, 9, 8, 7), Array(5, 6, 7, 8)))
    println(s2(Array(10, 9, 11, 12), Array(5, 6, 7, 8)))


  }
}
