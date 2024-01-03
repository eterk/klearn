package org.eter.klearn

import scala.collection.mutable

object RemoveBoxes546 {
  def remove(seq: Array[(Int, Int)]) = {
    seq.indexOf(seq.maxBy(_._2))
  }

  def reduce(seq: Array[(Int, Int)]): mutable.Seq[(Int, Int)] = {
    val r = collection.mutable.ArrayBuffer[(Int, Int)](seq.head)

    var i = 1
    while (i < seq.length) {
      val lastIndex = r.length - 1
      val (value, nums) = r(lastIndex)
      val (ov, oNums) = seq(i)
      if (value == ov) {
        r.update(lastIndex, (value, nums + oNums))
      } else {
        r.append((seq(i)))
      }

      i += 1
    }
    r

  }

  def reduce(seq: Array[Int]): mutable.Seq[(Int, Int)] = {
    val r = collection.mutable.ArrayBuffer[(Int, Int)]((seq.head, 1))
    var i = 1
    while (i < seq.length) {
      val lastIndex = r.length - 1
      val (value, nums) = r(lastIndex)
      if (value == seq(i)) {
        r.update(lastIndex, (value, nums + 1))
      } else {
        r.append((seq(i), 1))
      }

      i += 1
    }
    r
  }

  def s1(boxes: Array[Int]): Int = {
    val n = boxes.length // 盒子数量
    val dp = Array.ofDim[Int](n, n, n) // 动态规划数组
    def dfs(l: Int, r: Int, k: Int): Int = { // 定义递归函数dfs，l表示左边界，r表示右边界，k表示左边界左侧与左边界相同颜色的盒子数量
      if (l > r) return 0 // 如果左边界大于右边界，返回0
      if (dp(l)(r)(k) > 0) return dp(l)(r)(k) // 如果dp数组中已经有值，直接返回该值
      var j = l // 定义j为左边界
      var p = k // 定义p为左边界左侧与左边界相同颜色的盒子数量
      while (j < r && boxes(j + 1) == boxes(l)) { // 如果j小于右边界并且j+1位置上的盒子颜色与左边界位置上的盒子颜色相同，则j加一，p加一
        j += 1
        p += 1
      }
      var res = dfs(j + 1, r, 0) + (p + 1) * (p + 1) // 定义res为dfs(j+1,r,0)+(p+1)*(p+1)
      for (m <- j + 1 to r if boxes(m) == boxes(l)) { // 遍历j+1到r之间的盒子，如果该盒子颜色与左边界位置上的盒子颜色相同，则更新res的值为dfs(j+1,m-1,0)+dfs(m,r,p+1)和res中的最大值。
        res = res.max(dfs(j + 1, m - 1, 0) + dfs(m, r, p + 1))
      }
      dp(l)(r)(k) = res // 将res存入dp数组中
      res // 返回res的值
    }

    dfs(0, n - 1, 0)

  }

  def apply(boxes: Array[Int]): Int = {
    s1(boxes)
  }

  def main(args: Array[String]): Unit = {

    reduce(Array(1, 2, 4, 4, 2, 2, 5, 1)).foreach(println)
    println()
    reduce(Array(1, 3, 2, 2, 2, 3, 4, 3, 1).map((_, 1))).foreach(println)


    println(s1(Array(1, 3, 2, 2, 2, 3, 4, 3, 1))==23)
    println(s1(Array(1, 1, 1))==9)
    println(s1(Array(1))==1)
  }

}
