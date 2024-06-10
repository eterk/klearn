package org.eter.klearn
package sorted

object SingleNumber136 {


  def s1(nums: Array[Int]): Int = {
    val map = collection.mutable.Map[Int, Boolean]()
    var i = 0
    while (i < nums.length) {
      if (map.isDefinedAt(nums(i))) {
        map.remove(nums(i))
      } else {
        map.update(nums(i), true)
      }

      i += 1
    }
    map.keys.head
  }

  def s2(nums: Array[Int]): Int = {
    val x = nums.sorted
    var i = 1
    while (i < nums.length) {
      if (x(i) - x(i - 1) != 0) {
        return x(i - 1)
      }
      i += 2
    }
    x.last
  }

  /**
   * . The code you entered is a possible solution for Leetcode 137 using bitwise operations1.
   * It sorts the array using XOR swap2 and
   * then checks for the single number by comparing adjacent elements3.
   * However, this solution may not be optimal as it has a time complexity of O(n^2)
   * and a space complexity of O(1)
   * @param nums
   * @return
   */
  def s3(nums: Array[Int]): Int = {
    for (i <- nums.indices) {
      for (j <- (i + 1 until nums.length)) {
        if (nums(i) > nums(j)) {
          nums(i) = nums(j) ^ nums(i)
          nums(j) = nums(j) ^ nums(i)
          nums(i) = nums(j) ^ nums(i)
        }
      }
      if (i % 2 == 1) {
        if (nums(i) - nums(i - 1) != 0) {
          return nums(i - 1)
        }
      }
    }
    nums.last
  }

  def s4(nums: Array[Int]): Int = {
    var temp = 0
    var check = 1
    for (i <- nums.indices) {
      for (j <- (i + 1 until nums.length)) {
        if (nums(i) > nums(j)) {
          temp = nums(i)
          nums(i) = nums(j)
          nums(j) = temp
        }
      }
      if (i == check) {
        if (nums(i) - nums(i - 1) != 0) {
          return nums(i - 1)
        }
        check += 2
      }
    }
    nums.last
  }

  /**
   * bing 给的答案然而暂时无法通过测试
   *
   * 首先，我们定义两个变量a和b，分别用来存储出现一次和两次的数字。
   * 然后，我们遍历数组中的每个数字x，对于每个x，我们更新a和b的值。
   * 更新a的值时，我们使用异或运算（^），表示如果b中没有x，那么a中就加上x；如果b中有x，那么a中就去掉x。
   * 更新b的值时，我们使用与运算（&）和非运算（~），表示如果a中没有x，并且b中也没有x，那么b中就加上x；
   * 如果a中有x或者b中有x，那么b中就去掉x。
   * 最后，我们返回a的值作为答案。
   * */
  def s5(nums: Array[Int]): Int = {
    var a = 0 // 定义一个变量a，用来存储出现一次的数字
    var b = 0 // 定义一个变量b，用来存储出现两次的数字
    for (x <- nums) { // 遍历数组中的每个数字x
      a = a ^ x & ~b // 更新a的值，使用异或运算（^），表示如果b中没有x，那么a中就加上x；如果b中有x，那么a中就去掉x
      b = b ^ x & ~a // 更新b的值，使用与运算（&）和非运算（~），表示如果a中没有x，并且b中也没有x，那么b中就加上x；如果a中有x或者b中有x，那么b中就去掉x
    }
    a // 返回a的值作为答案
  }

  def apply(nums: Array[Int]): Int = {
    s3(nums)
  }

  def main(args: Array[String]): Unit = {

    //    println(apply(Array(2, 2, 1)))
    println(apply(Array(2, 3, 2, 4, 1, 3, 1)))
  }

}
