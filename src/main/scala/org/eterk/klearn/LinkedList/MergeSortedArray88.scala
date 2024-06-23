package org.eterk.klearn.LinkedList

import org.eterk.klearn.LeetCode


object MergeSortedArray88 extends LeetCode[(Array[Int], Int, Array[Int], Int), Array[Int]] {
  override def no: Int = 88

  override def desc: String = "给你两个有序整数数组 nums1 和 nums2，请你将 nums2 合并到 nums1 中，使 nums1 成为一个有序数组。"

  override def domain(input: (Array[Int], Int, Array[Int], Int)): Boolean = input._1.length >= 1 && input._1.length <= 200

  override def stdIO: Seq[((Array[Int], Int, Array[Int], Int), Array[Int])] = {
    Seq(
      ((Array(1, 2, 3, 0, 0, 0), 3, Array(2, 5, 6), 3), Array(1, 2, 2, 3, 5, 6)),
      ((Array(0), 0, Array(1), 1), Array(1)),
      ((Array(1), 1, Array(), 0), Array(1)),
      ((Array(2, 0), 1, Array(1), 1), Array(1, 2))
    )
  }

  def s1(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    val x = (nums1.take(m) ++ nums2.take(n)).sorted
    for (i <- nums1.indices) {
      nums1(i) = x(i)
    }
  }

  def merge1(nums1: Array[Int], m: Int, nums3: Array[Int], n: Int): Unit = {
    var nums2 = nums3

    if (nums2.isEmpty) {
      return ()
    }
    var index2 = 0
    var index1 = 0

    while (true) {
      print(index1 + "  " + index2 + " ")
      if (index1 == m || index2 == n) {

        while (index1 <= m + n - 1) {
          for (i <- nums2) {
            nums1(index1) = i
            index1 += 1
          }
        }

        println(nums2.mkString(","))
        println(nums1.mkString(","))

        return ()
      }
      if (nums2(index2) < nums1(index1)) {
        nums1(index1) = nums2(index2) ^ nums1(index1)
        nums2(index2) = nums2(index2) ^ nums1(index1)
        nums1(index1) = nums2(index2) ^ nums1(index1)
        index1 += 1
        nums2 = nums2.sorted
        println(nums1.mkString(",") + "  -> " + nums2.mkString(","))
      } else {
        index2 += 1
      }

    }


  }

  //todo  简单题没做出来....
  def m1(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Array[Int] = {
    if (m == 0) (0 until n).foreach(i => nums1(i) = nums2(i))
    val nums0 = nums1.take(m)
    var i = m
    var j = n
    while (i > 0 && j > 0) {
      val v2 = nums2(j - 1)
      val v1 = nums0(i - 1)
      if (v2 < v1) {
        nums1(i + j - 1) = v1
        i -= 1
      } else {
        nums1(i + j - 1) = v2
        j -= 1
      }
    }
    nums1

  }


  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Array[Int] = {
    (nums1.take(m) ++ nums2).sorted.zipWithIndex.foreach {
      case (v, i) => nums1(i) = v
    }
    nums1
  }

  /**
   * 实现类默认实现set 方法体为 Map("实例方法"->小驼峰命名的方法)
   */
  override def set: Map[String, ((Array[Int], Int, Array[Int], Int)) => Array[Int]] =
    Map(
      "默认方法" -> merge,
      "双指针" -> m1
    )
}

object RemoveElement27 extends LeetCode[(Array[Int], Int), Int] {
  override def no: Int = 27

  override def desc: String = "给你一个数组 nums 和一个值 val，你需要原地移除所有数值等于 val 的元素，并返回移除后数组的新长度。"

  override def domain(input: (Array[Int], Int)): Boolean = input._1.length >= 0 && input._1.length <= 100

  override def stdIO: Seq[((Array[Int], Int), Int)] = {
    Seq(
      ((Array(3, 2, 2, 3), 3), 2),
      ((Array(0, 1, 2, 2, 3, 0, 4, 2), 2), 5)
    )
  }

  override def set: Map[String, ((Array[Int], Int)) => Int] = {
    Map(
      "默认方法" -> removeElement
    )
  }

  def removeElement(nums: Array[Int], `val`: Int): Int = {
    var j = 0
    for (i <- nums.indices) {
      if (nums(i) != `val`) {
        nums(j) = nums(i)
        j += 1
      }
    }
    j
  }
}

object RemoveDuplicates26 extends LeetCode[Array[Int], Int] {
  override def no: Int = 26

  override def desc: String = "给你一个有序数组 nums ，请你原地删除重复出现的元素，使每个元素只出现一次 ，返回删除后数组的新长度。"

  override def domain(input: Array[Int]): Boolean = input.length >= 0

  override def stdIO: Seq[(Array[Int], Int)] = {
    Seq(
      (Array(1, 1, 2), 2),
      (Array(0, 0, 1, 1, 1, 2, 2, 3, 3, 4), 5)
    )
  }

  override def set: Map[String, Array[Int] => Int] = {
    Map(
      "默认方法" -> removeDuplicates
    )
  }

  def removeDuplicates(nums: Array[Int]): Int = {
    var j = 0
    var last = nums.head
    for (i <- nums.indices.tail) {
      if (nums(i) != last) {
        nums(j) = last
        j += 1
        last = nums(i)
      }
    }
    nums(j) = last
    j + 1
  }
}

object RemoveDuplicatesII80 extends LeetCode[Array[Int], Int] {
  override def no: Int = 80

  override def desc: String = "给你一个有序数组 nums ，请你原地删除重复出现的元素，使每个元素最多出现两次 ，返回删除后数组的新长度。"

  override def domain(input: Array[Int]): Boolean = input.length >= 1

  override def stdIO: Seq[(Array[Int], Int)] = {
    Seq(
      (Array(1, 1, 1, 2, 2, 3), 5),
      (Array(0, 0, 1, 1, 1, 1, 2, 3, 3), 7)
    )
  }

  override def set: Map[String, Array[Int] => Int] = {
    Map(
      "默认方法" -> removeDuplicates
    )
  }

  def removeDuplicates(nums: Array[Int]): Int = {
    var j = 0
    var num = 1
    var last = nums.head
    for (i <- nums.indices.tail) {
      val equal = nums(i) == last
      if (equal) {
        num += 1
      } else {
        num = 1
      }
      if (num <= 2) {
        nums(j) = last
        last = nums(i)
        j += 1
      }
    }
    println(nums.toSeq)
    nums(j) = last

    j + 1
  }
}

object MajorityElement169 extends LeetCode[Array[Int], Int] {
  override def no: Int = 169

  override def desc: String = "给你一个大小为 n 的数组，找出其中的多数元素。多数元素是指在数组中出现次数 大于 ⌊ n/2 ⌋ 的元素。"

  override def domain(input: Array[Int]): Boolean = input.length >= 1

  override def stdIO: Seq[(Array[Int], Int)] = {
    Seq(
      (Array(3, 2, 3), 3),
      (Array(2, 2, 1, 1, 1, 2, 2), 2)
    )
  }

  override def set: Map[String, Array[Int] => Int] = {
    Map(
      "默认方法" -> majorityElement
    )
  }

  def majorityElement(nums: Array[Int]): Int = {

    val n = nums.length / 2

    var majority = nums.head
    val map = collection.mutable.Map[Int, Int](majority -> 1)
    var flag = true
    var maxNum = 1
    var j = 1
    while (flag && j < nums.length) {
      val v = nums(j)
      if (map.isDefinedAt(v)) {
        map.update(v, map(v) + 1)
      } else {
        map.addOne(v -> 1)
      }
      if (maxNum < map(v)) {
        maxNum = map(v)
      }
      if (maxNum > n) {
        flag = false
        majority = v
      }
      j += 1
    }

    majority

  }
}


object BestTimeToBuyAndSellStock121 extends LeetCode[Array[Int], Int] {
  override def no: Int = 121

  override def desc: String = "给定一个数组，它的第 i 个元素是一支给定股票第 i 天的价格。如果你最多只允许完成一笔交易（即买入和卖出一支股票一次），设计一个算法来计算你所能获取的最大利润。"

  override def domain(input: Array[Int]): Boolean = input.length >= 1

  override def stdIO: Seq[(Array[Int], Int)] = {
    Seq(
      (Array(7, 1, 5, 3, 6, 4), 5),
      (Array(7, 6, 4, 3, 1), 0)
    )
  }

  override def set: Map[String, Array[Int] => Int] = {
    Map(
      "默认方法" -> maxProfit
    )
  }
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.isEmpty) return 0

    val dm = Array.fill(prices.length)(0)
    var minPrice = prices(0)

    for (i <- 1 until prices.length) {
      minPrice = Math.min(minPrice, prices(i))
      dm(i) = Math.max(dm(i - 1), prices(i) - minPrice)
    }

    dm(prices.length - 1)
  }

  def maxProfit1(prices: Array[Int]): Int = {

    val dm = Array.fill(prices.length)(Int.MinValue)
    for (i <- (1 until dm.length)) {
      for (j <- (0 until i)) {
        dm(i) = Math.max(prices(i) - prices(j), dm(i))
      }
    }
    dm(prices.length - 1)
  }
}


