package org.eterk.klearn.LinkedList

object ReverseList207 {


  def apply(head: ListNode): ListNode = {
    s2(head)
  }

  /**
   * 基本上网上的所有解决方案都是这种，就是将链表的两个current 和next 的关系转置成 previous 和current 的关系
   *
   * @param head
   * @return
   */
  def s1(head: ListNode): ListNode = {
    @annotation.tailrec
    def loop(curr: ListNode, prev: ListNode): ListNode = {
      if (curr == null) prev
      else {
        val next = curr.next
        curr.next = prev
        loop(next, curr)
      }
    }

    loop(head, null)
  }


  def s2(init: ListNode): ListNode = {
    /* iterative solution */
    var newHead: ListNode = null
    var head = init
    while (head != null) {
      val next: ListNode = head.next
      head.next = newHead
      newHead = head
      head = next
    }
    newHead
  }




}


