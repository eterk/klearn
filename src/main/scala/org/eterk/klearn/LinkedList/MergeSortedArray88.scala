package org.eterk.klearn.LinkedList

object MergeSortedArray88 {
  def s1(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    val x = (nums1.take(m) ++ nums2.take(n)).sorted
    for (i <- nums1.indices) {
      nums1(i) = x(i)
    }
  }

  def merge(nums1: Array[Int], m: Int, nums3: Array[Int], n: Int): Unit = {
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

  def main(args: Array[String]): Unit = {
    val m = (i: Array[Int], j: Array[Int]) => merge(i, i.length - j.length, j, j.length)
    //        m(Array(1, 2, 3, 0, 0, 0), Array(2, 5, 6))
    m(Array(1, 2, 3, 0, 0, 0), Array(2, 5, 6))
    //    m(Array(4, 5, 6, 0, 0, 0), Array(1, 2, 3))
    //    m(Array(1, 0), Array())
  }
}
