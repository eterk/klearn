package org.eter.klearn
package sorted

import scala.collection.mutable.ArrayBuffer


object RemoveKDigits402 {


  /**
   * 内存溢出
   */
  def s1(num: String, k: Int): String = {
    import scala.annotation.tailrec
    if (num.length == k) return "0"

    val removeI = (nums: String, i: Int) => nums.substring(0, i) ++ nums.substring(i + 1, nums.length)
    var cur = "0"

    @tailrec
    def getMin(nums: String, k: Int): String = {
      if (k == 0) return nums
      var i = 1
      var max = removeI(nums, 0)
      while (i < nums.length) {
        cur = removeI(nums, i)
        if (cur < max) max = cur
        i += 1
      }
      getMin(max, k - 1)
    }

    getMin(num, k).toInt.toString
  }

  /**
   *
   * 使用一个栈来存储数字，从左到右遍历字符串num。
   * 对于每个数字，如果栈不为空且栈顶元素大于当前数字，就弹出栈顶元素，并将k减一，
   * 直到栈为空或者k为零或者栈顶元素小于等于当前数字。
   * 将当前数字压入栈中。
   * 如果遍历完字符串后还有剩余的k，就从栈尾部删除k个元素。
   * 将栈中的元素转换成字符串，并去掉前导零。
   * 如果最终的字符串为空，就返回"0"。
   * 12032
   *
   */
  def s2(num: String, k: Int): String = {
    val stack = collection.mutable.Stack[Char]()
    var i = 0
    var kTemp = k
    while (i < num.length) {
      while (stack.nonEmpty && stack.top > num(i) && kTemp > 0) {
        stack.pop()
        kTemp -= 1
      }
      stack.push(num(i))
      i += 1
    }
    while (kTemp > 0) {
      stack.pop()
      kTemp -= 1
    }

    var res = ""
    while (stack.nonEmpty) {
      res = stack.pop() + res
    }

    while (res.length > 1 && res.head == '0') {
      res = res.tail
    }
    if (res == "") "0" else res

  }

  def s3(num: String, k: Int): String = {
    // 创建一个栈来存储数字
    val stack = collection.mutable.Stack[Char]()
    // 初始化一个指针和一个剩余的k值
    var i = 0
    var kTemp = k
    // 遍历字符串中的每个数字
    while (i < num.length) {
      // 如果栈不为空且栈顶元素大于当前数字，就弹出栈顶元素，并将kTemp减一，直到栈为空或者kTemp为零或者栈顶元素小于等于当前数字
      while (stack.nonEmpty && stack.top > num(i) && kTemp > 0) {
        stack.pop()
        kTemp -= 1
      }
      // 将当前数字压入栈中
      stack.push(num(i))
      i += 1
    }
    // 如果遍历完字符串后还有剩余的kTemp，就从栈尾部删除kTemp个元素
    while (kTemp > 0) {
      stack.pop()
      kTemp -= 1
    }

    // 创建一个StringBuilder对象来存储结果字符串
    var sb = new StringBuilder()

    // 使用while循环从后向前遍历stack并将其添加到sb中。
    // 这样做是因为stack是后进先出（LIFO）的数据结构，而我们需要按照先进先出（FIFO）的顺序输出结果。
    while (stack.nonEmpty) {
      sb.append(stack.pop())
    }

    // 将sb反转，使其按照正确的顺序输出
    sb = sb.reverse



    // 使用while循环去掉sb开头的所有'0'字符，直到sb为空或者第一个字符不是'0'
    while (sb.length > 1 && sb.charAt(0) == '0') {
      sb.deleteCharAt(0)
    }
    // 如果最终的字符串为空，就返回"0"
    if (sb.isEmpty) "0" else sb.toString()

  }

  def s4(num: String, k: Int): String = {

    // 创建一个可变的字符串对象
    val sb = new java.lang.StringBuilder(num)
    // 初始化一个指针和一个剩余的k值
    var i = 0
    var kTemp = k
    // 遍历字符串中的每个数字
    while (i < sb.length && kTemp > 0) {
      // 如果当前数字小于前一个数字，就删除前一个数字，并将指针后退一位
      if (i > 0 && sb.charAt(i) < sb.charAt(i - 1)) {
        sb.deleteCharAt(i - 1)
        i -= 1
        kTemp -= 1
      } else {
        // 否则，将指针向前移动一位
        i += 1
      }
    }
    while (kTemp > 0) {
      sb.deleteCharAt(sb.length() - 1)
      kTemp -= 1
    }

    // 使用while循环去掉sb开头的所有'0'字符，直到sb为空或者第一个字符不是'0'
    while (sb.length > 1 && sb.charAt(0) == '0') {
      sb.deleteCharAt(0)
    }

    // 如果最终的字符串为空，就返回"0"
    if (sb.length() == 0) "0" else sb.toString()

  }


  def apply(num: String, k: Int): String = {
    s4(num, k)
  }

  def main(args: Array[String]): Unit = {
    println(apply("12032", 3)) // 2
    println(apply("1432219", 3)) //1219
    println(apply("10200", 1)) //200
    println(apply("10001", 4)) //0
    println(apply("100", 2)) //0
    println(apply("100", 1)) //0
    println(apply("112", 1)) //0
    println(apply("10", 2)) //0
    println(apply("9", 1)) //0
    println(apply("1234567890", 5)) //0
  }


}
