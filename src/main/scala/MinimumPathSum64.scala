package org.eter.klearn

import scala.util.Try

object MinimumPathSum64 {

  def s1(grid: Array[Array[Int]]): Int = {
    val m = grid.length - 1
    val n = grid.head.length - 1

    for (i <- 0 to m) {
      for (j <- 0 to n) {
        val last: Int =
          (i, j) match {
            case (0, 0) => 0
            case (0, not0) if not0 != 0 => grid(0)(not0 - 1)
            case (not0, 0) if not0 != 0 => grid(not0 - 1)(0)
            case (i1, j1) => Math.min(grid(i1)(j1 - 1), grid(i1 - 1)(j1))
          }
        grid(i)(j) = last + grid(i)(j)
      }
    }
    grid(m)(n)
  }



  def apply(grid: Array[Array[Int]]): Int = {

    s1(grid)
  }

  def main(args: Array[String]): Unit = {

  }


}
