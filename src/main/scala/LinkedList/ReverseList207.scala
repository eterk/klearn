package org.eter.klearn
package LinkedList


object ReverseList207 {


  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

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

  def main(args: Array[String]): Unit = {
    val l5 = new ListNode(5, null)
    val l4 = new ListNode(4, l5)
    val l3 = new ListNode(3, l4)
    val l2 = new ListNode(2, l3)
    val l1 = new ListNode(1, l2)


    def show(r: ListNode): Unit = {
      println(r.x)
      if (r.next != null) {
        show(r.next)
      }
    }

    val r = ReverseList207(l1)
    show(r)


  }

}


