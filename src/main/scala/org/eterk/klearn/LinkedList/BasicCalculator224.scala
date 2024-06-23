package org.eterk.klearn.LinkedList

import scala.util.matching.Regex

/**
 * Constraints:
 *
 * 1 <= s.length <= 3 * 105
 * s consists of digits, '+', '-', '(', ')', and ' '.
 * s represents a valid expression.
 * '+' is not used as a unary operation (i.e., "+1" and "+(2 + 3)" is invalid).
 * '-' could be used as a unary operation (i.e., "-1" and "-(2 + 3)" is valid).
 * There will be no two consecutive operators in the input.
 * Every number and running calculation will fit in a signed 32-bit integer.
 */
object BasicCalculator224 {

  def apply(s: String): Int = {
    s1(s)
  }

  def s1(s: String): Int = {
    val expr = s.replaceAll(" ", "")
    // Define a regular expression pattern for integers
    val intPattern: Regex = "^\\d+".r
    val plusPattern = "^\\+\\d+".r
    val minusPattern = "^-\\d+".r

    // Define a function that takes a string and returns an Int and a String
    def extractIntAndRemaining(pattern: Regex, s: String): (Int, String) = {
      // Extract the integer from the beginning of the string using findFirstIn method
      val number = intPattern.findFirstIn(s)
      // Check if the number is defined or not
      if (number.isDefined) {
        // Convert the number to Int using toInt method
        val result = number.get.toInt
        // Replace the number with an empty string using replaceFirstIn method
        val remaining = intPattern.replaceFirstIn(s, "")
        // Return the result and the remaining string as a tuple
        (result, remaining)
      } else {
        // Throw an exception or return a default value if no integer is found
        //      throw new NumberFormatException("No integer found at the beginning of the string")
        // Or return (0, s) or some other value as default
        (0, s)
      }
    }

    def cac(str: String, r: Int): Int = {
      str match {
        case empty if empty.isEmpty => r + 0
        case sp if sp.startsWith("(") => cac(sp.tail, 0)
        case spp if spp.startsWith("+(") => r + cac(spp.tail.tail, 0)
        case spm if spm.startsWith("-(") => r - cac(spm.tail.tail, 0)
        case ep if ep.startsWith(")") => cac(ep.tail, r)
        case pp if plusPattern.findPrefixOf(pp).isDefined =>
          val (num, tail) = extractIntAndRemaining(intPattern, pp.tail)
          r + num + cac(tail, 0)
        case mp if minusPattern.findPrefixOf(mp).isDefined =>
          val (num, tail) = extractIntAndRemaining(intPattern, mp.tail)
          r - num + cac(tail, 0)
        case number => val (num, tail) = extractIntAndRemaining(intPattern, number)
          cac(tail, num)
      }
    }

    cac(expr, 0)
  }

  def s2(s: String): Int = {
    val stack = scala.collection.mutable.Stack[Int]() // 创建一个栈来存储之前的结果和符号
    var sign = 1 // 初始化符号为正数
    var res = 0 // 初始化结果为0
    var i = 0 // 初始化索引为0
    while (i < s.length) { // 遍历字符串
      if (s(i).isDigit) { // 如果当前字符是数字
        var num = s(i) - '0' // 计算数字的值
        while (i + 1 < s.length && s(i + 1).isDigit) { // 如果下一个字符也是数字，继续计算值
          num = num * 10 + s(i + 1) - '0'
          i += 1
        }
        res += sign * num // 根据符号更新结果
      } else if (s(i) == '+') { // 如果当前字符是加号
        sign = 1 // 设置符号为正数
      } else if (s(i) == '-') { // 如果当前字符是减号
        sign = -1 // 设置符号为负数
      } else if (s(i) == '(') { // 如果当前字符是左括号
        stack.push(res) // 把之前的结果压入栈中
        stack.push(sign) // 把之前的符号压入栈中
        res = 0 // 重置结果为0
        sign = 1 // 重置符号为正数
      } else if (s(i) == ')') { // 如果当前字符是右括号
        res *= stack.pop() // 把结果乘以栈顶的符号
        res += stack.pop() // 把结果加上栈顶的之前的结果
      }
      i += 1 // 索引加一
    }
    res // 返回最终结果
  }


  def main(args: Array[String]): Unit = {
    //    println(s1("1+2"))
    //    println(s1("1+2+(3-2)"))
    //    println(s1("1-(4+(3-2))"))
    //    println(s1("2-1+2"))
    //    println(s1("2+1-2+1"))
    //    println(s1("2-1+2"))
    //    println(s1("(1+(4+5+2)-3)+(6+8)"))
    println(s2("(7)-(0)+(4)"))
  }


}
