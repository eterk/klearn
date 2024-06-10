package org.eter.klearn
package queue

object DesignCircularDeque {

  class MyCircularDeque(_k: Int) {
    var context = Seq.empty[Int]


    def isFull(): Boolean = {
      _k == context.length
    }

    def insertFront(value: Int): Boolean = {
      if (isFull()) {
        false
      } else {
        context = value +: context

        true
      }

    }

    def insertLast(value: Int): Boolean = {
      if (isFull()) {
        false
      } else {
        context = context :+ value
        true
      }
    }

    def deleteFront(): Boolean = {
      if (isEmpty()) {
        false
      } else {
        context = context.tail
        true
      }

    }

    def deleteLast(): Boolean = {
      if (isEmpty()) {
        false
      } else {
        context = context.dropRight(1)
        true
      }

    }

    def getFront(): Int = {

      if (isEmpty()) {
        -1
      } else {
        context.head
      }
    }

    def getRear(): Int = {
      if (isEmpty()) {
        -1
      } else {
        context.last
      }

    }

    def isEmpty(): Boolean = {
      context.isEmpty
    }


  }

  class MyCircularDeque1(var _k: Int) {

    /** Initialize your data structure here. Set the size of the deque to be k. */
    var head = DoubleListNode(-1)
    var tail = DoubleListNode(-1)
    head.pre = tail
    tail.next = head
    var size = 0


    /** Adds an item at the front of Deque. Return true if the operation is successful. */
    def insertFront(value: Int): Boolean = {
      if (isFull) return false
      val node = new DoubleListNode(value)
      node.next = head
      node.pre = head.pre
      head.pre.next = node
      head.pre = node
      size += 1
      true
    }

    /** Adds an item at the rear of Deque. Return true if the operation is successful. */
    def insertLast(value: Int): Boolean = {
      if (isFull) return false
      val node = new DoubleListNode(value)
      node.next = tail.next
      tail.next.pre = node
      tail.next = node
      node.pre = tail
      size += 1
      true
    }

    /** Deletes an item from the front of Deque. Return true if the operation is successful. */
    def deleteFront: Boolean = {
      if (size == 0) return false
      head.pre.pre.next = head
      head.pre = head.pre.pre
      size -= 1
      true
    }

    /** Deletes an item from the rear of Deque. Return true if the operation is successful. */
    def deleteLast: Boolean = {
      if (size == 0) return false
      tail.next.next.pre = tail
      tail.next = tail.next.next
      size -= 1
      true
    }

    /** Get the front item from the deque. */
    def getFront = head.pre.x

    /** Get the last item from the deque. */
    def getRear = tail.next.x

    /** Checks whether the circular deque is empty or not. */
    def isEmpty = size == 0

    /** Checks whether the circular deque is full or not. */
    def isFull = size == _k
  }

  case class DoubleListNode(var x: Int) {
    var pre: DoubleListNode = _
    var next: DoubleListNode = _
  }


}
