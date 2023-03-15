package org.eter.klearn
package sorted

object MaxArea11 {

  def apply(height: Array[Int]): Int = {
    s1(height)
  }

  /**
   * 寻找最高的两个数与他们之间的索引差来计算容积
   * 寻找次高的两个数来计算他们的索引差
   * 共有 C(height.length,2)可能性。寻找这些组合中的最大值// 这个时暴力求解，时间复杂度太高
   * 先将 height 排序，从小到大。取最大的两个
   *
   */
  def s1(height: Array[Int]): Int = {

    // 每一个点的极限值 （最大索引-当前索引）*当前点的值
    // 当有一个区间的值大于点的极限值，那么该点pass
    //当有一个区间pass 了所有极限值，那么该区间就是结果
    var i = 0

    val pointLimit = new Array[Int](height.length)

    while (i < height.length) {
      pointLimit(i) = Math.max(height.length - i - 1, i) * height(i)
      i += 1
    }
    i = 0
    var res = 0

    while (i < height.length) {

      if (res < pointLimit(i)) {
        //        println(s"$i: ${pointLimit(i)} $res")
        val startIndex: Int = res / height(i)
        for (j <- (height.length - 1 to startIndex by -1)) {
          //          println(j)
          val current = Math.min(height(i), height(j)) * (j - i)
          if (current > res) res = current
          //          loopTime += 1
        }
        //        println()
      }

      i += 1
    }


    //    println(loopTime)

    res
  }

  // 内存溢出的
  def s2(height: Array[Int]): Int = {
    var i = 0
    var res = 0

    while (i < height.length) {
      val startIndex: Int = if (height(i) == 0) 1 else res / height(i)
      for (j <- (height.length - 1 to startIndex by -1)) {
        //          println(j)
        val current = Math.min(height(i), height(j)) * (j - i)
        if (current > res) res = current
      }
      i += 1
    }

    res
  }

  def s3(height: Array[Int]): Int = {
    var i = 0
    var res = 0
    val pointLimit = new Array[Int](height.length)

    while (i < height.length) {
      pointLimit(i) = Math.max(height.length - i - 1, i) * height(i)
      if (res < pointLimit(i)) {
        //        println(s"$i: ${pointLimit(i)} $res")
        val startIndex: Int = res / height(i)
        for (j <- (height.length - 1 to startIndex by -1)) {
          //          println(j)
          val current = Math.min(height(i), height(j)) * (j - i)
          if (current > res) res = current
        }
      }
      i += 1
    }
    res
  }

  def s4(height: Array[Int]): Int = {
    var i = 0
    var j = height.length - 1

    var water = 0
    while (i < j) {
      water = Math.max(water, (j - i) * Math.min(height(i), height(j)))
      if (height(i) < height(j)) {
        i += 1
      } else {
        j -= 1
      }
    }
    water
  }


  def main(args: Array[String]): Unit = {
    val max = 100000

    //    val arr = Array(1, 8, 6, 2, 5, 4, 8, 3, 7)

    val arr = ((max to 0 by -1) ++ (0 to max)).toArray

    println(s1(arr))

    println(s1(Array(1, 1)))

  }
}
