package org.eter.klearn
package LinkedList


import scala.collection.mutable

/**
 * 方法一：暴力法。对于每个’‘，尝试三种可能性：’(', ‘)’, ‘’。然后检查每种情况下的字符串是否有效。这种方法需要遍历所有可能性，时间复杂度是O(N3^N)，
 * 空间复杂度是O(N)，其中N是字符串长度。
 * 方法二：贪心法。维护两个变量low和high，分别表示在任意位置左括号的数量可能的最小值和最大值。遍历字符串，如果遇到左括号，则low和high都加一；
 * 如果遇到右括号，则low和high都减一；如果遇到’*'，则low减一，high加一。在任意时刻，如果low小于0，则将它设为0；如果high小于0，
 * 则说明无论如何都无法匹配所有右括号，返回false。最后检查low是否等于0，如果是，则说明可以匹配所有左右括号，返回true；否则返回false。
 * 这种方法只需要遍历一次字符串，时间复杂度是O(N)，空间复杂度是O(1)。
 * 方法三：栈法。使用两个栈分别存储左括号和星号的索引。遍历字符串，如果遇到左括号，则将其索引压入第一个栈；如果遇到星号，则将其索引压入第二个栈；
 * 如果遇到右括号，则优先弹出第一个栈中的元素（如果有），否则弹出第二个栈中的元素（如果有），否则返回false。
 * 最后检查两个栈中剩余元素是否能够匹配：当第一个栈不为空时，弹出其顶部元素，并尝试从第二个栈中弹出一个比它大的元素（即在它之后出现的星号），
 * 如果没有找到，则返回false；否则继续循环直到第一个栈为空或者第二个栈为空。最后检查第一个栈是否为空，如果是，则返回true；否则返回false
 */
object ValidParentheses20 {
  def apply(s: String): Boolean = {

    s1(s)
  }

  /**
   * Runtime495 ms Beats 93.36% Memory54.6 MB Beats 64.60%
   */
  // s1方法：用栈来判断括号是否匹配
  def s1(s: String): Boolean = {
    // 如果字符串为空或者长度为0，返回false
    if (s == null || s.isEmpty) return false
    // 定义一个左括号和右括号的映射关系
    val leftMatch = Map[Char, Char]('[' -> ']', '(' -> ')', '{' -> '}')
    // 反转映射关系，得到右括号和左括号的映射关系
    val rightMatch = leftMatch.map(_.swap)

    // 创建一个可变的栈
    val stack = new mutable.Stack[Char]()

    // 遍历字符串中的每个字符
    for (c <- s) {
      // 如果字符是左括号，就入栈
      if (leftMatch.isDefinedAt(c)) {
        stack.push(c)
      }
      // 如果字符是右括号，就出栈并比较是否匹配
      if (rightMatch.isDefinedAt(c)) {

        if (stack.isEmpty || leftMatch(stack.pop()) != c) return false
      }
    }
    // 如果栈为空，说明所有的括号都匹配了，返回true；否则返回false
    stack.isEmpty
  }

  import scala.collection.mutable

  // s2方法：用字节序列和栈来判断括号是否匹配
  def s2(s: String): Boolean = {

    // 定义一个函数，将字符串中的字符转换为字节序列，其中正数表示左括号，负数表示右括号
    def reCode(s: String): Seq[Byte] = {
      s.map {
        case '[' => 1: Byte
        case ']' => -1: Byte
        case '(' => 2: Byte
        case ')' => -2: Byte
        case '{' => 3: Byte
        case '}' => -3: Byte

      }

    }

    // 定义一个递归函数，判断栈是否为空或者是否匹配
    def isLegal(stack: mutable.Stack[Byte], e: Seq[Byte]): Boolean = {
      e match {
        case l +: tail if l > 0 =>
          stack.push(l)
          isLegal(stack, tail)
        case r +: tail if r < 0 =>
          if (stack.isEmpty) {
            false
          } else {
            if (stack.pop() + r == 0) {
              isLegal(stack, tail)
            } else {
              false
            }
          }
        case Nil =>
          stack.isEmpty

      }

    }

    // 根据字符串的长度进行不同的处理
    s.size match {
      case 0 => true // 如果长度为0，返回true
      case 1 => false // 如果长度为1，返回false
      case _ =>
        val byteSeq = reCode(s) // 将字符串转换为字节序列
        isLegal(mutable.Stack(byteSeq.head), byteSeq.tail) // 调用递归函数判断是否合法

    }

  }


  // Runtime530 ms Beats 59.29% Memory52.4 MB Beats 98.67%
  def s3(s: String): Boolean = {
    // Define a map of matching brackets
    val pairs = Map(')' -> '(', '}' -> '{', ']' -> '[')
    val values = pairs.values.toSeq
    // Declare an empty stack
    var stack = List[Char]()
    // Iterate through the string
    for (c <- s) {
      // If c is an opening bracket, push it to the stack

      if (values.contains(c)) {
        stack = c :: stack
      }
      // If c is a closing bracket, check if the stack is empty or not matching
      else if (stack.isEmpty || stack.head != pairs(c)) {
        // If yes, return False
        return false
      }
      // If c is a closing bracket and matches the stack top, pop it from the stack
      else {
        stack = stack.tail
      }
    }
    // Return True if the stack is empty, False otherwise
    return stack.isEmpty
  }


}
