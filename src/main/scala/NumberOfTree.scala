package org.eter.klearn

object NumberOfTree {

  def s1(n: Int): Int = {
    val dp = Array.fill(n + 1)(0)
    dp(0) = 1
    dp(1) = 1
    for (i <- 2 to n) {
      for (j <- 1 to i) {
        dp(i) += dp(j - 1) * dp(i - j)
      }
    }
    dp(n)
  }

  import scala.math.log10

  def minHeight(n: Int): Int = {
    // 计算以2为底的对数
    val log2n = log10(n + 1) / log10(2)
    // 向上取整
    val height = math.ceil(log2n).toInt
    height
  }

  def apply(nodes: Int): Int = {
    s1(nodes)
  }

  def minHeight1(n: Int): Int = {
    var height = 0
    var nodes = 1
    while (nodes <= n) {
      height += 1
      nodes = nodes * 2 + 1
    }
    height
  }

  def main(args: Array[String]): Unit = {

    println(apply(5))
    println(apply(4))
  }

}

