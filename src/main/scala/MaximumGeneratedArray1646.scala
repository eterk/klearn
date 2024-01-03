package org.eter.klearn

object MaximumGeneratedArray1646 {

  def s1(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    val res = new Array[Int](n + 1)
    res(0) = 0
    res(1) = 1
    var max = 1
    var j = 2
    while (j <= n) {
      val i = j / 2
      if (j % 2 != 0) {
        res(j) = res(i) + res(i + 1)
      } else {
        res(j) = res(i)
      }
      max = Math.max(max, res(j))
      j += 1
    }
    println(res.mkString(","))

    max
  }

  def s2(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    val nums = new Array[Int](n + 1)
    nums(0) = 0
    nums(1) = 1
    var ans = 1
    var i = 1
    while ((2 * i + 1) <= n) {
      nums(2 * i) = nums(i)
      nums(2 * i + 1) = nums(i) + nums(i + 1)
      ans = Math.max(ans, nums(2 * i + 1))

      i += 1
    }
    println(nums.mkString(","))
    ans
  }


  def apply(n: Int): Int = {
    s2(n)
  }

  def main(args: Array[String]): Unit = {

    println(apply(50) == 3)
//    println()
//    println(apply(3) == 2)
//    println(apply(0) == 0)
  }

}
