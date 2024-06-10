package org.eter.klearn
package stack

object DailyTemperatures739 {

  def apply(temperatures: Array[Int]): Array[Int] = {

    s1(temperatures)
  }

  def s0(temperatures: Array[Int]): Array[Int] = {

    def isR(head: Int, tail: Array[Int]): Array[Int] = {
      if (tail.isEmpty) return Array()
      var r = tail.indexWhere(x => x > head)
      r = if (r <= 0) 0 else r + 1
      r +: isR(tail.head, tail.tail)
    }

    isR(temperatures.head, temperatures.tail)

  }

  def s1(temperatures: Array[Int]): Array[Int] = {

    val res = collection.mutable.ArrayBuffer[Int]()

    for (i <- temperatures.indices) {
      val r = temperatures.indexWhere(x => x > temperatures(i), i) - i
      res.append {
        if (r < 0) 0 else r
      }
    }

    res.toArray
  }

  def s2(temperatures: Array[Int]): Array[Int] = {
    temperatures.zipWithIndex.map {
      case (k, v) =>
        val r = temperatures.indexWhere(x => x > k, v) - v
        if (r < 0) 0 else r
    }
  }

  def s3(t: Array[Int]): Array[Int] = {
    // 获取数组t的长度，并赋值给变量n
    val n = t.length
    // 创建一个新的整数数组answer，它的长度和t相同，并初始化为0
    val answer = new Array[Int](n)
    // 创建一个可变的栈stack，用来存储数组t中的元素下标
    val stack = scala.collection.mutable.Stack[Int]()
    // 从0开始遍历数组t中的每个元素，用变量i表示当前元素的下标
    for (i <- 0 until n) {
      // 当栈不为空，并且栈顶元素对应的数组t中的值小于当前元素时，执行以下操作：
      while (stack.nonEmpty && t(stack.top) < t(i)) {
        // 弹出栈顶元素，并赋值给变量j
        val j = stack.pop()
        // 计算当前元素和弹出元素之间的距离，并赋值给answer数组中对应位置
        answer(j) = i - j
      }
      // 将当前元素的下标压入栈中
      stack.push(i)
    }
    // 返回answer数组作为函数结果
    answer
  }


  def main(args: Array[String]): Unit = {

    println(s3(Array(73, 74, 75, 71, 69, 72, 76, 73)).mkString(","))

  }

}
