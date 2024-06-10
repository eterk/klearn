package org.eter.klearn
package sorted


object wiggleMaxLength376 {
  def s1(nums: Array[Int]): Int = {
    import scala.collection.mutable
    val stack = new mutable.Stack[Int]()
    var i = 1
    var currentLast = nums.head
    while (i < nums.length) {
      val diffNow = nums(i) - currentLast
      if (stack.nonEmpty) {
        if (diffNow < 0 && stack.top < 0) {
          if (stack.top > diffNow) {
            stack.pop()
            currentLast = nums(i)
            stack.push(diffNow)
          }
        } else if (diffNow > 0 && stack.top > 0) {
          if (stack.top < diffNow) {
            stack.pop()
            currentLast = nums(i)
            stack.push(diffNow)
          }
        } else {
          currentLast = nums(i)
          stack.push(diffNow)
        }
      } else {
        currentLast = nums(i)
        stack.push(diffNow)
      }
      println(stack)

      i += 1
    }
    //    println(stack.size + "  " + stack)
    stack.size + 1
  }

  def apply(nums: Array[Int]): Int = {

    s1(nums)
  }

  def main(args: Array[String]): Unit = {

    //    println(apply(Array(1, 7, 4, 9, 2, 5)) == 6)
    //    16, -7, 3, -3, 6, -8
    //    println(apply(Array(1, 17, 5, 10, 13, 15, 10, 5, 16, 8)) == 7)
    //    println(apply((1 to 9).toArray) == 2)
  }
}
