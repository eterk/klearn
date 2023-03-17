package org.eter.klearn
package sorted

import scala.collection.mutable.ListBuffer

object ThreeSum15 {

  // 暴力求解  ：内存溢出
  def s1(nums: Array[Int]): List[List[Int]] = {

    var i = 0
    val res = collection.mutable.Set[List[Int]]()
    while (i < nums.length) {
      val current = nums(i)
      for (j <- 0 until i) {
        for (k <- 0 until j) {
          if ((i > j) && (j > k)) {
            if (j != k && (nums(j) + nums(k) == -current)) {
              res.addOne(List(current, nums(j), nums(k)).sorted)
            }
          }
        }
      }
      i += 1
    }

    res.toList

  }

  // 内存溢出
  def s2(nums: Array[Int]): List[List[Int]] = {
    var i = 0
    val res = collection.mutable.Map[Int, collection.mutable.Set[(Int, Int, Int, Int)]]()
    var j = 0
    while (i < nums.length) {
      j = i + 1
      while (j < nums.length) {
        val v =
          if (nums(i) < nums(j)) {
            (i, j, nums(i), nums(j))
          } else {
            (i, j, nums(j), nums(i))
          }

        val k = -(v._3 + v._4)
        if (res.isDefinedAt(k)) {
          res.update(k, res(k).addOne(v))
        } else {
          res.update(k, collection.mutable.Set(v))
        }
        j += 1

      }
      i += 1
    }
    i = 0

    val result = collection.mutable.Set[List[Int]]()
    while (res.nonEmpty && i < nums.length) {
      if (res.isDefinedAt(nums(i))) {
        val r = res(nums(i))
        r.foreach {
          case (j, k, a, b) if (j != i && i != k) => result.add(List(nums(i), a, b).sorted)
          case _ =>
        }
      }

      i += 1
    }

    result.toList

  }


  def apply(nums: Array[Int]): List[List[Int]] = {
    s2(nums)
  }


  def main(args: Array[String]): Unit = {

    apply(Array(-1, 0, 1, 2, -1, -4)).foreach(println)


    apply(Array(-1, 0, 1, -1, -1, 1)).foreach(println)


    apply(Array(0, 0, 0)).foreach(println)

    apply(Array(0, 1, 1)).foreach(println)
  }


}
