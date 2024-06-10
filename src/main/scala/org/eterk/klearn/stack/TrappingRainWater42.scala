package org.eter.klearn
package stack

object TrappingRainWater42 {
  def apply(height: Array[Int]): Int = {
    s1(height)
  }

  def s1(height: Array[Int]): Int = {
    // 开始位置
    var startIndex = 0
    var short = 0
    var r = 0
    while (startIndex < height.length) {
      var i = startIndex + 1
      while (i < height.length) {
        if (height(i) >= height(startIndex)) {
          short = Math.min(height(i), height(startIndex))
          val increase = short * (i - startIndex - 1) - height.slice(startIndex + 1, i).sum
          println(s"$short $startIndex --> $i : $increase " + height.slice(startIndex + 1, i).mkString(","))
          r += increase
          startIndex = i
        }
        i += 1
      }
      startIndex += 1
    }
    r
  }


  /**
   * 对于一个柱状图，遍历每一个横坐标，
   * 使用栈来存储当前的索引
   * 当遇到下个位置的柱子高于前一个的柱子的时候
   * 开始遍历栈，每次只计算i-2到i之间能存储的水量无视更前面的高柱子带来的溢出值*
   */
  def s3(height: Array[Int]): Int = {
    import scala.collection.mutable.Stack
    val stack = Stack[Int]() // 创建一个栈，用来存放数组的索引
    var ans = 0 // 初始化结果为0
    for (i <- height.indices) { // 遍历数组的每个元素
      while (stack.nonEmpty && height(i) > height(stack.top)) { // 当栈不为空且当前元素大于栈顶元素时，说明可以积水了
        val top = stack.pop() // 弹出栈顶元素，作为积水的底部位置
        if (stack.isEmpty) {
          // 如果此时栈为空了，说明没有左边界了，无法积水了（这种情况只会发生在第一个元素大于第二个元素时）
          ()
        } else {
          val distance = i - stack.top - 1 // 计算积水的宽度（当前位置减去新的栈顶位置再减一）
          val boundedHeight = math.min(height(i), height(stack.top)) - height(top) // 计算积水的高度（新旧栈顶位置对应的高度中较小者减去底部位置对应的高度）
          println(s"i=$i dis=$distance bounded=$boundedHeight")
          ans += distance * boundedHeight // 更新结果（宽度乘以高度）
        }
      }
      stack.push(i) // 将当前位置入栈（保持单调递减）
    }
    ans // 返回结果
  }

  // 效率高，但是不太懂
  def s4(height: Array[Int]): Int = {
    import scala.math.max
    def helper(l: Int, r: Int, level: Int, rst: Int): Int = {
      if (l >= r) { // 如果左右指针相遇或交叉，结束递归
        rst // 返回结果
      } else {
        (l, r) match {
          case (l, r) if (height(l) <= height(r)) => // 如果左边的高度小于等于右边的高度，说明左边是短板
            helper(l + 1, r, max(level, height(l)), rst + max(0, level - height(l))) // 移动左指针，更新水平线和结果
          case (l, r) if (height(l) > height(r)) => // 如果左边的高度大于右边的高度，说明右边是短板
            helper(l, r - 1, max(level, height(r)), rst + max(0, level - height(r))) // 移动右指针，更新水平线和结果
        }
      }
    }

    helper(0, height.length - 1, -1, 0) // 初始时，左右指针分别在数组两端，水平线和结果为-1（任意负数即可）
  }




  def main(args: Array[String]): Unit = {

    println(s3(Array(4, 2, 0, 3, 2, 5)))
    println(s4(Array(4, 2, 0, 3, 2, 5)))
    //    println(s1(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)))
    //    println(s2(Array(4, 2, 3)))
    //    println(s3(Array(6, 4, 2, 0, 3, 2, 0, 3, 1, 4, 5, 3, 2, 7, 5, 3, 0, 1, 2, 1, 3, 4, 6, 8, 1, 3)))

  }
}
