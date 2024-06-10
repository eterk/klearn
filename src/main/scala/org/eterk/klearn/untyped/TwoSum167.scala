package org.eterk.klearn.untyped

object TwoSum167 extends LeetCode[(Array[Int], Int), Array[Int]] {
  override def no: Int = 167

  override def desc: String = "给你一个下标从 1 开始的整数数组 numbers ，该数组已按 非递减顺序排列 ，请你从数组中找出满足相加之和等于目标数 target 的两个数。如果设这两个数分别是 numbers[index1] 和 numbers[index2] ，则 1 <= index1 < index2 <= numbers.length 。"

  override def domain(input: (Array[Int], Int)): Boolean = {
    val (numbers, target) = input
    2 <= numbers.length && numbers.length <= 3 * 104 &&
      numbers.forall(n => -1000 <= n && n <= 1000) &&
      numbers.sliding(2).forall(pair => pair(0) <= pair(1)) &&
      -1000 <= target && target <= 1000
  }

  override def stdIO: Seq[((Array[Int], Int), Array[Int])] = Seq(
    ((Array(2, 7, 11, 15), 9), Array(1, 2)),
    ((Array(2, 3, 4), 6), Array(1, 3)),
    ((Array(-1, 0), -1), Array(1, 2))
  )

  override def set: Map[String, ((Array[Int], Int)) => Array[Int]] = Map("双指针" -> (twoSum _).tupled)

  /**
   * 从两头向中间遍历
   */
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    val nums = 1 +: numbers
    var (i, j) = (1, numbers.length)
    while (i <= j) {
      val current = nums(i) + nums(j)
      if (current < target) {
        i += 1
      } else if (current > target) {
        j -= 1
      } else {
        return Array(i, j)
      }
    }
    Array.emptyIntArray
  }
}

