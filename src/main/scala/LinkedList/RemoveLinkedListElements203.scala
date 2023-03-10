package org.eter.klearn
package LinkedList

object RemoveLinkedListElements203 {
  def apply(head: ListNode, ele: Int): ListNode = {
    s1(head, ele)
  }


  def s1(head: ListNode, ele: Int): ListNode = {

    val dup = ListNode(-1, head)
    var cur = head
    var pre = dup
    while (cur != null) {
      if (cur.x == ele) {
        pre.next = cur.next
      } else {
        pre = cur
      }
      cur = cur.next
    }
    dup.next
  }

  def s2(head: ListNode, ele: Int): ListNode = {
    val dummy = new ListNode()
    dummy.next = head

    @annotation.tailrec
    def go(pre: ListNode, next: ListNode): Unit = {
      if (next != null) {
        if (next.x == ele) {
          pre.next = next.next
          if (next.next != null) {
            go(pre, next.next)
          }
        } else {
          go(next, next.next)
        }
      }
    }

    go(dummy, dummy.next)

    dummy.next
  }

}
