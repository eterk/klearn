package org.eter.klearn
package sorted


/**
 * leetcode 16 的题目是求最接近 target 的三数之和。一个简单的方法是用三层循环遍历所有可能的组合，
 * 然后比较它们与 target 的差值的绝对值，更新最小的差值和对应的三数之和。
 * 但是这个方法的时间复杂度是 O(n^3)，不够高效。
 * 一个更好的方法是先对数组进行排序，然后用双指针法来寻找最接近 target 的三数之和。
 * 具体步骤如下：
 * 对数组进行排序，从小到大。
 * 初始化一个变量 res 为数组中前三个元素之和。
 * 遍历数组，对于每个元素 nums[i]，在它右边的区间 [i+1, n-1] 中用双指针法寻找两个元素 nums[j] 和 nums[k]，使得 nums[i] + nums[j] + nums[k] 最接近 target。
 * 如果 nums[i] + nums[j] + nums[k] 等于 target，直接返回这个值。
 * 否则，比较它们与 target 的差值的绝对值与当前 res 与 target 的差值的绝对值，如果更小，则更新 res 为 nums[i] + nums[j] + nums[k]。
 * 根据 nums[i] + nums[j] + nums[k] 与 target 的大小关系，移动 j 或 k 指针，缩小搜索范围。
 * 返回 res。
 * 这个方法的时间复杂度是 O(n^2)，空间复杂度是 O(1)
 * * */
object ThreeSumClosest16 {
  /**
   * 暴力解法
   */
  def s1(nums: Array[Int], target: Int): Int = {

    var min = Int.MaxValue
    for (i <- (0 to nums.length - 3)) {
      for (j <- (i + 1 to nums.length - 2)) {
        for (k <- j + 1 until nums.length) {
          val current = (nums(i) + nums(j) + nums(k))
          if (current == target) {
            return target
          }
          else if (Math.abs(current - target) < Math.abs(min - target)) {
            min = current
          }
        }
      }
    }

    min

  }

  def s2(numsSrc: Array[Int], target: Int): Int = {
    val nums = numsSrc.sorted

    var min = nums.take(3).sum

    for (i <- nums.indices) {
      var j = i + 1
      var k = nums.length - 1
      while (j < k) {
        val current = nums(i) + nums(j) + nums(k)
        //        println(s" $i $j $k :$current")
        if (current == target) {
          return target
        } else if (current > target) {
          k -= 1
        } else {
          j += 1
        }
        if (Math.abs(current - target) < Math.abs(min - target)) {
          min = current
        }
      }
    }

    min

  }

  def s3(nums: Array[Int], target: Int): Int = {
    // 对数组进行排序
    val sorted = nums.sorted
    // 初始化结果为数组中前三个元素之和
    var res = sorted(0) + sorted(1) + sorted(2)
    // 遍历数组
    for (i <- sorted.indices) {
      // 定义两个指针，分别指向当前元素右边的区间的首尾
      var j = i + 1
      var k = sorted.length - 1
      // 当两个指针没有相遇时，循环执行以下操作
      while (j < k) {
        // 计算当前三个元素之和
        val sum = sorted(i) + sorted(j) + sorted(k)
        // 如果三数之和等于目标值，直接返回这个值
        if (sum == target) return sum
        // 否则，比较它们与目标值的差值的绝对值与当前结果与目标值的差值的绝对值，如果更小，则更新结果为三数之和
        if (math.abs(sum - target) < math.abs(res - target)) res = sum
        // 根据三数之和与目标值的大小关系，移动左指针或右指针，缩小搜索范围
        if (sum < target) j += 1 else k -= 1
      }
    }
    // 返回结果
    res
  }


  def apply(nums: Array[Int], target: Int): Int = {

    s3(nums, target)
  }

  def main(args: Array[String]): Unit = {

    println(apply(Array(-1, 2, 1, -4), 1))
    println(apply(Array(0, 0, 0), 1))
  }
}
