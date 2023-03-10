package org.eter.klearn
package LinkedList

object OddEvenLinkedList328 {

  def apply(head: ListNode): ListNode = {
    s1(head)
  }

  def s1(head: ListNode): ListNode = {

    var i = 1

    var cur = head

    val isOdd = () => i % 2 == 0
    val evenHead = ListNode(-1, null)
    var even = evenHead
    val oddHead = ListNode(-1, null)
    var odd = oddHead

    while (cur != null) {

      if (isOdd()) {
        odd.next = ListNode(cur.x, null)
        odd = odd.next
      } else {
        even.next = ListNode(cur.x, null)
        even = even.next
      }

      cur = cur.next
      i += 1
    }

    if (oddHead.next != null) {
      even.next = oddHead.next
    }

    evenHead.next

  }

  def main(args: Array[String]): Unit = {
    val head = ListNode((1 to 5).toArray)
    println(ListNode(Array(0)))
    println(ListNode(Array[Int]()))
    println(s1(head))
  }

}
