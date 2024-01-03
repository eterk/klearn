package org.eter.klearn
package sorted

object SearchRange34 {


  def s1(nums: Array[Int], target: Int): Array[Int] = {
    val res = Array[Int](-1, -1)
    if (nums.isEmpty) return res
    var i = 0
    var findingHead = true
    var findingLast = true

    var last = nums.length - 1 - i

    while ((findingHead && i < nums.length) || (findingLast && last > 0)) {
      //      println(i + "-> " + last)
      if (findingHead && nums(i) == target) {
        //        println(res(0))
        res(0) = i
        findingHead = false
      }
      if (findingLast && nums(last) == target) {
        res(1) = last
        findingLast = false
      }
      if (findingHead) {
        i += 1
      }

      if (findingLast) {
        last -= 1
      }

    }
    res
  }

  def apply(nums: Array[Int], target: Int): Array[Int] = {
    val r = s1(nums, target)
    println(r.mkString(","))
    r
  }

  def main(args: Array[String]): Unit = {
    //
    apply(Array(5, 7, 7, 8, 8, 10), 8)
    apply(Array(5, 7, 7, 8, 8, 10), 6)
    apply(Array(), 6)
    apply(Array(1, 2, 3), 1)
    apply(Array(1, 2, 2, 3, 4, 4, 4), 4)


  }

}
