package org.eter.klearn
package LinkedList

object OfferReversePrint {

  def apply(head: ListNode): Array[Int] = {
    s1(head)

  }

  def s1(head: ListNode): Array[Int] = {
    val stack = collection.mutable.Stack[Int]()
    var cur = head

    while (cur != null) {
      stack.push(cur.x)
      cur = cur.next
    }

    stack.popAll().toArray


  }
}