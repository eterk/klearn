package org.eterk.klearn.untyped

import scala.annotation.tailrec

object Test20241009 {


  /**
   * leetcode 300
   * 使用动态规划
   */
  def lengthOfLIS(nums: Array[Int]): Int = {
    val dp = Array.fill(nums.length)(1)
    var max = 1
    nums.indices.tail.foreach(i => {

      (0 until i).foreach(j => {
        if (nums(i) > nums(j)) {
          dp(i) = Math.max(dp(j) + 1, dp(i))
        }
      })
      max = Math.max(dp(i), max)

    })
    max
  }

  /**
   * 139
   */
  def trailingZeroes(n: Int): Int = {

    var num = n
    var count = 0
    while (num > 0) {
      num = num / 5
      count += num
    }
    count

  }

  def jiecheng(n: Int): Int = {
    val res = Array.empty[Int]
    val toIntArr = (num: Int) => n.toString.map(_.toString.toInt).reverse


    n
  }

  def factorial(n: Int): Array[Int] = {
    // 初始化结果数组，初始值为1
    var result = Array(1)

    // 逐步计算阶乘
    for (i <- 2 to n) {
      result = multiply(result, i)
    }

    result
  }

  // 用于将数组表示的大数与一个整数相乘 todo  我自己写不出来
  def multiply(num: Array[Int], x: Int): Array[Int] = {
    var carry = 0
    var result = Array.empty[Int]

    for (digit <- num.reverse) {
      val product = digit * x + carry
      result = (product % 10) +: result
      carry = product / 10
    }

    while (carry > 0) {
      result = (carry % 10) +: result
      carry /= 10
    }

    result
  }

  import scala.annotation.tailrec

  def mySqrt(x: Int): Int = {
    var l = 0
    var r = x
    var ans = -1
    while (l <= r) {
      println(s"a: $l $ans $r ")
      val mid = l + (r - l) / 2;
      if (mid.toLong * mid <= x) {
        ans = mid;
        l = mid + 1;
      } else {
        r = mid - 1;
      }
    }
    ans
  }

  def minSubArrayLen(target: Int, nums: Array[Int]): Int = {
    var i = 0

    val countBuffer = collection.mutable.Buffer.empty[Int]

    while (i < nums.length) {
      var j = i + 1
      while (j < nums.length && j >= i) {
        println(i + "  " + j)
        val num = nums.slice(i, j)
        val sum = num.sum
        if (sum < target) {
          j += 1
        } else if (sum == target) {
          println(s"$i $j : ${nums.slice(i, j).mkString(",")}")
          countBuffer.append(j - i)
        } else {
          i += 1
        }

      }
      i += 1

    }
    println(countBuffer.mkString(","))
    if (countBuffer.isEmpty) {
      0
    } else {
      countBuffer.min
    }


  }


  def plusOne(digits: Array[Int]): Array[Int] = {
    var seq = digits

    @tailrec
    def plus(index: Int): Unit = {
      if (index == -1)
        return seq = 1 +: seq

      val value = seq(index) + 1
      if (value >= 10) {
        seq(index) = 0
        plus(index - 1)

      } else {
        seq(index) = value
      }

    }

    plus(seq.length - 1)
    seq


  }


  def test300() = {
    println(lengthOfLIS(Array(1, 2, 3, 2, 1)))
    println(lengthOfLIS(Array(1, 2, 3, 1, 2, 2, 4, 2, 1)))
    println(lengthOfLIS(Array(1, 1, 1, 1)))
    println(lengthOfLIS(Array(0, 1, 0, 3, 2, 3)))
    println(lengthOfLIS(Array(10, 9, 2, 5, 3, 7, 101, 18)))
  }


  def main(args: Array[String]): Unit = {


    //    println(plusOne(Array(9)).mkString(","))

    //    println(mySqrt(8))
    //    println(mySqrt(4))
    //    println(mySqrt(2147395599))
    println(minSubArrayLen(7, Array(1, 2, 4, 3, 7)))

    //    println(trailingZeroes(3))
    //    println(trailingZeroes(5))
    //    println(trailingZeroes(18))
    //    println(factorial(18).mkString(""))
    //    println(factorial(13).mkString(""))


  }
}

