package org.eterk.klearn.untyped

object TwoSum1 extends LeetCode[(Array[Int], Int), Array[Int]] {

  override def no: Int = 1

  override def desc: String = "给定一个整数数组 nums 和一个整数目标值 target，请你在该数组中找出 和为目标值 target 的那 两个 整数，并返回它们的数组下标。你可以假设每种输入只会对应一个答案。但是，数组中同一个元素在答案里不能重复出现。你可以按任意顺序返回答案。"

  override def domain(input: (Array[Int], Int)): Boolean = {
    val (nums, target) = input
    2 <= nums.length && nums.length <= 104 && nums.forall(num => -109 <= num && num <= 109) && -109 <= target && target <= 109
  }

  override def stdIO: Seq[((Array[Int], Int), Array[Int])] = Seq(
    (((Array(2, 7, 11, 15)), 9), Array(1, 0)),
    (((Array(3, 2, 4)), 6), Array(2, 1)),
    (((Array(3, 3)), 6), Array(1, 0))
  )

  override def set: Map[String, ((Array[Int], Int)) => Array[Int]] = Map("哈希表" -> (twoSum _).tupled)

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val map = collection.mutable.Map.empty[Int, Int]
    val res = new Array[Int](2)
    import scala.util.control.Breaks._

    breakable {
      for (i <- nums.indices) {
        val k = nums(i)
        if (map.isDefinedAt(target - k)) {
          res(0) = i
          res(1) = map(target - k)
          break
        } else {
          map.put(k, i)
        }
      }
    }
    res
  }
}
