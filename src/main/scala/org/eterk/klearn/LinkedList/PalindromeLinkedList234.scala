package org.eterk.klearn.LinkedList

import scala.collection.mutable

object PalindromeLinkedList234 {

  def apply(head: ListNode): Boolean = {
    s1(head)
  }

  def s1(head: ListNode): Boolean = {
    var cur = head
    val container = mutable.Buffer.empty[Int]
    while (cur != null) {
      container.append(cur.x)
      cur = cur.next
    }


    var i = 0
    var j: Int = -11
    while (true) {
      j = container.size - i - 1

      if (i == j) {
        return true
      } else {

        if (container(i) != container(j)) {
          return false
        }
      }
      if (Math.abs(i - j) == 1) {
        return true
      }
      i += 1
    }


    false

  }


  def main(args: Array[String]): Unit = {
    val arr = (0 to 9).toArray
    println(s1(ListNode(arr ++ arr.reverse)))


  }


}
