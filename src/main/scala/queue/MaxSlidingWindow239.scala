package org.eter.klearn
package queue

object MaxSlidingWindow239 {

  def apply(nums: Array[Int], k: Int): Array[Int] = {
    s1(nums, k)
  }

  def s1(nums: Array[Int], k: Int): Array[Int] = {
    if (k == 1) return nums
    var maxQueue = collection.mutable.Buffer.fill(k)(Int.MinValue)

    val res = collection.mutable.Buffer.empty[Int]
    for (i <- nums.indices) {
      maxQueue.append(nums(i))
      var j: Int = if (i - k + 1 < 0) i else i - k + 1
      while (j <= i) {
        maxQueue(j) = Math.max(nums(i), maxQueue(j))
        j += 1
        println(maxQueue.mkString(","))
      }
      println()
      //      if (i > nums.length-k + 1) {
      //
      //        res.append(maxQueue.head)
      //        maxQueue = maxQueue.tail
      //      }

    }
    maxQueue.toArray
  }

  /**
   *  暴力解法，数据量大的时候就内存溢出了
   * @param nums
   * @param k
   * @return
   */
  def s2(nums: Array[Int], k: Int): Array[Int] = {
    if (k == 1) return nums
    val maxQueue = collection.mutable.Buffer.fill(nums.length)(Int.MinValue)
    for (i <- nums.indices) {
      val maxIndex = if (i + k - 1 < nums.length - 1) i + k - 1 else nums.length - 1
      for (j <- i to maxIndex) {

        if (maxQueue(j) < nums(i)) {
          maxQueue(j) = nums(i)
        }
      }
    }

    maxQueue.slice(k - 1, nums.length).toArray
  }


  def main(args: Array[String]): Unit = {

    println(s2(Array(1, 3, -1, -3, 5, 3, 6, 7), 3).mkString(","))
  }

}
