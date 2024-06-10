package org.eterk.klearn.LinkedList

import scala.collection.mutable


object CountSmaller315 {
  def apply(nums: Array[Int]): List[Int] = {

    s1(nums)
  }

  def s1(nums: Array[Int]): List[Int] = {

    def getNums(cur: Int, nums: Array[Int]): Int = {
      var r = 0
      for (i <- nums) {
        if (i < cur) {
          r += 1
        }
      }
      r
    }

    def rec(nums: Array[Int]): List[Int] = {
      if (nums.isEmpty) return Nil
      getNums(nums.head, nums.tail) +: rec(nums.tail)

    }

    rec(nums)
  }

  def s2(nums: Array[Int]): List[Int] = {

    val map = new SortedMap(nums.last)
    val res =
      nums.dropRight(1)
        .foldRight(List[Int](0)) {
          (k, res) =>
            val count = map.count(k)
            map.appendOrUpdate(k)
            count +: res
        }
    res
  }


  class SortedMap(last: Int) {

    def toString(k: Int): String = {
      "k:" + k + "\n" +
        "count:" + countIndex(k) + "\n" +
        "append:" + appendIndex(k) + "\n" +
        "buffer:" + buffer.mkString(",") + "\n" +
        "key:" + sortKey.mkString(",") + "\n" +
        "count:" + count(k)
    }

    val buffer: mutable.Map[Int, Int] = mutable.Map[Int, Int](last -> 1)

    val sortKey: mutable.Buffer[Int] = mutable.Buffer(last)

    def append(k: Int): Unit = {
      sortKey.insert(appendIndex(k), k)
      buffer.update(k, 1)
    }

    def appendIndex(k: Int): Int = {
      countIndex(k) + 1
    }

    def countIndex(k: Int): Int = {
      val i = sortKey.indexWhere(_ > k)


      i match {
        case -1 => //比所有的都大
        case eq if sortKey(i) == k => eq - 1
        case other => other
      }


      if (i == -1) {
        if (k < sortKey.head) {
          0
        } else {
          sortKey.length - 1
        }

      } else {
        i - 1
      }

    }

    def update(k: Int): Unit = {
      buffer.update(k, buffer(k) + 1)
    }

    def appendOrUpdate(k: Int): Unit = {
      if (buffer.isDefinedAt(k)) {
        update(k)
      } else {
        append(k)
      }
    }

    /**
     * 计算小于K的数目
     */
    def count(k: Int): Int = {
      var r = 0
      val countI = appendIndex(k)

      for (i <- 0 until countI) {
        if (sortKey(i) < k) {
          r += buffer(sortKey(i))
        }

      }

      r
    }

  }


  def main(args: Array[String]): Unit = {
    println(s2(Array(5, 2, 6, 1)).mkString(","))
    println(s2(Array(2, 2, 2, 1)).mkString(","))
    println(s2(Array(2, 2, 2, 5)).mkString(","))
    println(s2(Array(2, 5, 2, 5)).mkString(","))
    println(s2((0 to 7).toArray).mkString(","))
    println(s2((0 to 7).toArray.reverse).mkString(","))
  }
}
