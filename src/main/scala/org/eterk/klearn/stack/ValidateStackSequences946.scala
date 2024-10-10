package org.eter.klearn
package stack

import scala.collection.mutable

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
