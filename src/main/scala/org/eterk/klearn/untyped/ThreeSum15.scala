package org.eterk.klearn.untyped

object ThreeSum15 extends LeetCode[Array[Int], List[List[Int]]] {

  override def no: Int = 15

  override def desc: String = "三数之和"

  override def domain(input: Array[Int]): Boolean = {
    3 <= input.length && input.length <= 3000 && input.forall(x => -105 <= x && x <= 105)
  }

  override def stdIO: Seq[(Array[Int], List[List[Int]])] = Seq(
    (Array(-1, 0, 1, 2, -1, -4), List(List(-1, 0, 1), List(-1, -1, 2))),
    (Array(0, 1, 1), List.empty),
    (Array(3, 0, -2, -1, 1, 2), List(List(-2, -1, 3), List(-2, 0, 2), List(-1, 0, 1)).reverse),
    (Array(0, 0, 0), List(List(0, 0, 0)))
  )


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


  override def set: Map[String, Array[Int] => List[List[Int]]] = {
    Map("双指针" -> threeSum)

  }

  def sortThreeNumbers(a: Int, b: Int, c: Int): List[Int] = {
    val min = a min b min c
    val max = a max b max c
    val mid = 0 - min - max
    List(min, mid, max)
  }

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val sortedNums = nums.sorted
    val res = collection.mutable.Set.empty[List[Int]]

    for (i <- sortedNums.indices) {
      if (i == 0 || sortedNums(i) != sortedNums(i - 1)) { // 避免重复
        var (lo, hi) = (i + 1, sortedNums.length - 1)
        while (lo < hi) {
          val sum = sortedNums(i) + sortedNums(lo) + sortedNums(hi)
          if (sum == 0) {
            res.add(List(sortedNums(i), sortedNums(lo), sortedNums(hi)))
            while (lo < hi && sortedNums(lo) == sortedNums(lo + 1)) lo += 1 // 避免重复
            while (lo < hi && sortedNums(hi) == sortedNums(hi - 1)) hi -= 1 // 避免重复
            lo += 1
            hi -= 1
          } else if (sum < 0) {
            lo += 1
          } else {
            hi -= 1
          }
        }
      }
    }

    res.toList
  }


  /**
   * stackover flow  n^3
   * @param nums
   * @return
   */
  def threeSumFailure(nums: Array[Int]): List[List[Int]] = {
    if (nums.length < 3) return List.empty

    var i = 0

    val res = collection.mutable.Set.empty[List[Int]]
    while (i < nums.length - 2) {
      var j = i + 1
      while (j < nums.length) {
        val first = nums(i)
        val second = nums(j)
        val third = 0 - first - second
        nums.slice(j + 1, nums.length).find(_ == third).foreach(i => res.add(sortThreeNumbers(first, second, third)))
        j += 1
      }
      i += 1
    }
    res.toList
  }
}
