package org.eter.klearn
package stack

object ClimbingStairs70 {
  /**
   * 使用动态规划法求解
   */
  def s1(n: Int): Int = {
    val dp = new Array[Int](n + 1)
    dp(0) = 1
    dp(1) = 2
    var i = 2
    while (i < n) {
      dp(i) = dp(i - 1) + dp(i - 2)
      i += 1
    }
    dp(n - 1)
  }

  def apply(n: Int): Int = {
    s1(n)
  }

  def main(args: Array[String]): Unit = {

    println(apply(4))
  }

}
