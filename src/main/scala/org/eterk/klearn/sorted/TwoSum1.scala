package org.eter.klearn
package sorted

object TwoSum1 {

  def apply(nums: Array[Int], target: Int): Array[Int] = {

    s1(nums, target)
  }

  def s1(nums: Array[Int], target: Int): Array[Int] = {
    var i = 0
    val map = collection.mutable.Map[Int, Int]()
    while (i < nums.length) {
      val minus = target - nums(i)
      if (map.isDefinedAt(nums(i))) {
        return Array(i, map(nums(i)))
      }
      map.update(minus, i)
      i += 1
    }
    Array.empty[Int]
  }

  import scala.collection.mutable.ArrayBuffer

  def s2(nums: Array[Int], target: Int): Array[Int] = {
    val result = ArrayBuffer[Array[Int]]()
    for (i <- nums.indices) {
      val t = target - nums(i)
      for (j <- i + 1 until nums.length) {
        if (nums(j) == t) {
          result.append(Array(i, j))
        }
      }
    }
    result(0)
  }


  def main(args: Array[String]): Unit = {
    //    println(s1(Array(2, 7, 11, 15), 9).mkString(","))
    println(s1(Array(3, 2, 4), 6).mkString(","))
  }

}
