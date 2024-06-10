package org.eterk.klearn.untyped

object LongestConsecutiveSeq128 extends LeetCode[Array[Int], Int] {

  override def no: Int = 128

  override def desc: String = "给定一个未排序的整数数组 nums ，找出数字连续的最长序列（不要求序列元素在原数组中连续）的长度。请你设计并实现时间复杂度为 O(n) 的算法解决此问题。"

  override def domain(input: Array[Int]): Boolean = {
    0 <= input.length && input.length <= 105 && input.forall(num => -109 <= num && num <= 109)
  }

  override def stdIO: Seq[(Array[Int], Int)] = Seq(
    ((Array(100, 4, 200, 1, 3, 2)), 4),
    ((Array(0)), 1),
    ((Array(0, 3, 7, 2, 5, 8, 4, 6, 0, 1)), 9)
  )

  override def set: Map[String, Array[Int] => Int] = Map("排序-遍历" -> longestConsecutive, "哈希" -> hasMap)

  def hasMap(nums: Array[Int]): Int = {
    val set = nums.toSet
    var maxLen = 0
    for (i <- nums) {
      var len = 0
      if (!set.contains(i - 1)) {
        len += 1
        var current = i
        while (set.contains(current + 1)) {
          len += 1
          current += 1
        }
      }
      maxLen = Math.max(len, maxLen)
    }

    maxLen
  }

  /**
   * 这个经过了排序,所以时n(排序算法的时间复杂度),不符合题目要求
   *
   */
  def longestConsecutive(nums: Array[Int]): Int = {
    if (nums.isEmpty) return 0
    val sortedNum = nums.sorted
    val map = collection.mutable.Map[Int, Int]()
    var (len, maxLen) = (1, 1)
    var lastN = sortedNum.head
    for (i <- sortedNum.tail) {
      i match {
        case g1 if g1 - 1 == lastN =>
          len += 1
        case eq if eq == lastN =>
        case other => len = 1
      }

      if (maxLen < len) {
        maxLen = len
      }
      lastN = i
    }

    maxLen
  }
}


