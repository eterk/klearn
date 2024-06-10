package org.eterk.klearn.find

object BinarySearch704 {


  def s1(nums: Array[Int], target: Int): Int = {

    def find(nums: Array[Int], from: Int, to: Int): Int = {
      if (from == to && nums(from) != target) return -1
      val mid: Int = (to - from) / 2 + from

      //      println(s"$from $to=> $mid")
      if (nums(mid) == target) {
        return mid
      } else if (nums(mid) < target) {

        find(nums, Math.min(mid + 1, to), to)
      } else if (nums(mid) > target) {
        find(nums, from, Math.max(from, mid - 1))
      } else {
        -1
      }
      return -1
    }

    find(nums, 0, nums.length - 1)
  }

  def s2(nums: Array[Int], target: Int): Int = {
    val n: Int = nums.length - 1
    var low = 0
    var high: Int = n
    while ( {
      low <= high
    }) {
      val mid = low + (high - low) / 2
      if (nums(mid) == target) return mid
      else if (nums(mid) > target) high = mid - 1
      else low = mid + 1
    }
    -1
  }

  def apply(nums: Array[Int], target: Int): Int = {

    s2(nums, target)
  }

  def main(args: Array[String]): Unit = {


    println(apply(Array(1, 2, 4, 8, 90), 9))
    println(apply(Array(-1, 0, 3, 5, 9, 12), 9))
    println(apply(Array(1, 2, 4, 8, 90), 8))
    println(apply(Array(-1, 0, 5), -1))
    println(apply(Array(5), 5))

  }

}
