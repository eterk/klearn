package org.eter.klearn
package LinkedList

object MinStack155 {


  class MinStack1() {
    var container: Seq[Int] = Seq[Int]()

    def push(i: Int) {
      container = i +: container

    }

    def pop() {
      container = container.tail
    }

    def top(): Int = {
      container.head
    }

    def getMin(): Int = {
      container.min
    }


  }

  class ListNode(val x: Int, var previous: ListNode)

  class MinStack2() {

    var topNode: ListNode = _

    def push(i: Int) {
      topNode = new ListNode(i, topNode)
    }

    def pop() {
      topNode = topNode.previous
    }

    def top(): Int = {
      topNode.x
    }

    def getMin(): Int = {
      var tmp = topNode
      var min = topNode.x
      while (tmp.previous != null) {
        tmp = tmp.previous
        if (min > tmp.x) {
          min = tmp.x
        }
      }
      min

    }


  }


  class MinStack3() {
    // use two stacks to store elements and min values
    val stack = new scala.collection.mutable.Stack[Int]()
    val minStack = new scala.collection.mutable.Stack[Int]()

    def push(x: Int) {
      // push x to stack
      stack.push(x)
      // if minStack is empty or x is smaller than or equal to the current min, push x to minStack
      if (minStack.isEmpty || x <= minStack.top) {
        minStack.push(x)
      }
    }

    def pop() {
      // pop from stack
      val x = stack.pop()
      // if x is equal to the current min, pop from minStack
      if (x == minStack.top) {
        minStack.pop()
      }
    }

    def top(): Int = {
      // return the top of stack
      stack.top
    }

    def getMin(): Int = {
      // return the top of minStack
      minStack.top
    }
  }

}

