package org.eter.klearn
package sorted

object JumpGame55 {
  /**
   * The algorithm is a greedy approach that tries to find the next best jump at each step.
   * It has a time complexity of O(n^2) and a space complexity of O(1),
   * where n is the length of the array.
   * A possible improvement is to use a single loop and
   * keep track of the farthest reachable position at each step,
   * which can reduce the time complexity to O(n).
   */
  def s1(nums: Array[Int]): Boolean = {
    val max = nums.length - 1 // the last index of the array
    var i = 0 // the current index

    while (i <= max) { // loop until reaching the end or returning false
      val need = max - i // the distance needed to reach the end from current index
      var maxDis = 0 // the maximum distance that can be reached from current index
      var maxI = i // the next index that gives the maximum distance
      for (jump <- 0 to nums(i)) { // loop through all possible jumps from current index
        val curDis = nums(i + jump) + jump // the distance that can be reached from jumping to i + jump
        if (maxDis < curDis) { // update the maximum distance and next index if needed
          maxDis = curDis
          maxI = i + jump
        }
        if (maxDis >= need) return true // return true if reaching or exceeding the end
      }
      if (i == maxI) {
        return false // return false if stuck at current index (cannot jump further)
      } else {
        i = maxI // update current index to next index
      }

    }

    false // return false if reaching here (should not happen)
  }

  /**
   * leetcode 中其他人的回答，使用回溯算法
   * The algorithm is a greedy approach that tries to find the earliest position
   * that can reach the end at each step.
   * It has a time complexity of O(n) and
   * a space complexity of O(1), where n is the length of the array.
   * This is an improvement over your previous solution in terms of time complexity.
   */
  def s2(nums: Array[Int]): Boolean = {
    var last = nums.length - 1 // the last index of the array
    var i = nums.length - 2 // start from the second last index
    while (i >= 0) { // loop backward until reaching the first index or returning true
      if (i + nums(i) >= last) last = i // update the last index if current index can jump to or beyond it
      i -= 1 // move to the previous index
    }
    last <= 0 // return true if the last index is zero or negative (meaning the first index can reach the end)
  }


  def apply(nums: Array[Int]): Boolean = {

    s2(nums)
  }

  def main(args: Array[String]): Unit = {

    println(apply(Array(2, 3, 1, 1, 4)))
    println(apply(Array(3, 2, 1, 0, 4)))
    println(apply(Array(3, 0, 2, 0, 4)))
    println(apply(Array(1, 0, 2, 0, 4)))
    println(apply(Array(3, 0, 0, 0, 0)))

  }

}
