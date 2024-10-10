package org.eter.klearn
package stack

import org.eterk.klearn.LeetCode

import scala.collection.mutable
import scala.util.Try

object ValidateStackSequences946 {


  def apply(pushed: Array[Int], popped: Array[Int]): Boolean = {

    s1(pushed, popped)
  }

  /**
   * 经过多次调试后拼写完成  50% 50%
   *
   * @return
   */
  def s1(pushed: Array[Int], popped: Array[Int]): Boolean = {

    var i = 0 // initialize a variable to keep track of the index of pushed array
    val container = new mutable.Stack[Int]() // create a mutable stack to simulate the push and pop operations
    var j = 0 // initialize a variable to keep track of the index of popped array
    var currentJ = popped.head // get the first element of popped array
    var top = pushed.head // get the first element of pushed array

    while (i < pushed.length) { // loop through the pushed array
      container.push(pushed(i)) // push the current element to the stack
      top = container.top // update the top element of the stack
      while (currentJ == top) { // loop while the current element of popped array matches the top element of the stack
        container.pop() // pop the top element from the stack
        if (container.nonEmpty) {
          top = container.top // update the top element if the stack is not empty
        }
        if (j < popped.length - 1) {
          j += 1 // increment j if it is not at the end of popped array
          currentJ = popped(j) // update the current element of popped array
        } else {
          top = -1 // set top to -1 if j reaches the end of popped array
        }

      }

      i += 1 // increment i after each iteration
    }
    container.isEmpty // return true if the stack is empty after looping through both arrays, or false otherwise
  }


  /**
   * 50% 100%
   */
  def s2(pushed: Array[Int], popped: Array[Int]): Boolean = {

    @annotation.tailrec
    def dfs(stack: Seq[Int], idx: Int): (Seq[Int], Int) = {
      if (idx >= pushed.length || !stack.headOption.contains(popped(idx))) (stack, idx)
      else dfs(stack.tail, idx + 1)
    }
    /* define a tail recursive function that takes a stack and an index as parameters
       and returns a tuple of updated stack and index
       if the head option of the stack does not match with
       or does not exist for
       corresponding value in popped array at given index,
       return them as they are,
       otherwise call itself recursively with tail of stack and incremented index */
    pushed.foldLeft(Seq.empty[Int], 0) {
      /* use foldLeft method on pushed array with an initial value
                                               consisting an empty sequence as stack and zero as index */
      case ((stack, idx), n) => dfs(n +: stack, idx)
      /* for each element n in pushed array,
         call dfs function with n prepended to current stack and current index */
    }._1.isEmpty /* return true if final updated stack is empty after folding through all elements in pushed array,
                   or false otherwise */
  }

  /**
   * 100% 100%
   */
  def s3(pushed: Array[Int], popped: Array[Int]): Boolean = {
    var i = 0 // Intialise one pointer pointing on pushed array
    var j = 0 // Intialise one pointer pointing on popped array

    //    import scala.collection.JavaConversions._
    for (v <- pushed) {
      pushed({
        i += 1;
        i - 1
      }) = v // using pushed as the stack.

      while ( {
        i > 0 && (pushed(i - 1) == popped(j))
      }) { // pushed[i - 1] values equal to popped[j];
        i -= 1 // decrement i

        j += 1 // increment j

      }
    }
    i == 0 // Since pushed is a permutation of popped so at the end we are supposed to be left with an empty stack

  }

  def main(args: Array[String]): Unit = {

    println(s3(Array(1, 2, 3, 4, 5), Array(4, 5, 3, 2, 1)))


  }


}

object BasicCalculator224 extends LeetCode[String, Int] {
  override def no: Int = 224

  override def desc: String = "给你一个字符串表达式 s ，请你实现一个基本计算器来计算并返回它的值。"

  override def domain(input: String): Boolean = input.nonEmpty

  override def stdIO: Seq[(String, Int)] = {
    Seq(
      ("1 + 1", 2),
      (" 2-1 + 2 ", 3),
      ("-(-2)", 2),
      ("1-(-2)", 3),
      ("-(-2+2)", 0),
      ("(-2+2)", 0),
      ("(1+(4-(5 +2)+ 3))+(6+8)", 15),
      ("2-4-(8+2-6+(8+4-(1)+8-10))", -15),
      ("(1+( 4+5+ 2)-3)+(6+8)", 23)
    )
  }

  override def set: Map[String, String => Int] = {
    Map(
      "默认方法" -> calculate1
    )
  }

  def minusPlus(mid: String): Int = {

    val tmp = new StringBuilder("")
    var plus = 1
    val str = mid.replaceAll("--", "+")
    val iter = str.iterator
    var res = 0
    while (iter.hasNext) {
      iter.next match {
        case '+' =>
          val last =
            if (tmp.isEmpty) {
              0
            } else {
              tmp.result().toInt
            }
          res = res + plus * last
          tmp.clear()
          plus = 1
        case '-' =>
          if (tmp.isEmpty) {
            tmp.append('-')
          } else {
            res = res + (plus * tmp.result().toInt)
            tmp.clear()
            plus = -1
          }

        case o => tmp.append(o)
      }
    }
    res = res + (plus * tmp.result().toInt)
    res
  }

  def calculate(s: String): Int = {
    var str = s.replaceAll(" ", "")
    var res = 0
    var flag = true

    while (flag) {
      if (str.contains('(')) {
        val b = str.lastIndexOf('(')
        val e = str.indexOf(')', b)
        val mid = str.slice(b + 1, e)
        val res = minusPlus(mid)
        str = str.slice(0, b) + res + str.slice(e, str.length)
      } else {
        res = minusPlus(str.replaceAll("\\)", ""))
        flag = false
      }
    }
    res
  }

  def calculate1(src: String): Int = {
    val plus = (a: Int, b: Int) => {
      println(a + " + " + b)
      a + b
    }
    val minus = (a: Int, b: Int) => {
      println(a + " - " + b)
      a - b
    }
    val s = src.replaceAll(" ", "")

    def state(i: Int, tmp: StringBuilder): Int = {

      val sta = if (i >= 2) {
        s.slice(i - 2, i + 1) match {
          case "-(-" => 0
          case _ => 1
        }
      } else {
        1
      }
      if (sta == 1) {
        if (tmp.isEmpty) {
          1
        } else {
          2
        }
      } else {
        0
      }

    }

    var tmp = new StringBuilder("")

    def rec(from: Int, resInit: Int, signInit: (Int, Int) => Int): Int = {
      if (s.isEmpty) return resInit
      var res = resInit
      var sign = signInit

      def compute(state: Int): Unit = {
        val lastValue = state match {
          case 0 => tmp.addOne('-')
          case 1 =>
          case 2 =>
            println(tmp.toString().toInt)
            res = sign(res, tmp.toString().toInt)
            tmp.clear()
        }

      }

      for (i <- (from until s.length)) {

        val sta = state(i, tmp)
        s(i) match {
          case '(' => signInit(resInit, rec(i + 1, res, sign))
          case ')' => compute(sta)
          case '+' => compute(sta)
            sign = plus
          case '-' =>
            compute(sta)
            sign = minus
          case o =>
            tmp.append(o)
        }
        if (i == s.length - 1) {
          compute(state(i, tmp))
        }
      }

      res
    }

    rec(0, 0, plus)
  }


}
