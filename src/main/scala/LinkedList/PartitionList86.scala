package org.eter.klearn
package LinkedList

import scala.collection.mutable

object PartitionList86 {

  def apply(head: ListNode, x: Int): ListNode = {

    s1(head, x)
  }

  def s1(head: ListNode, x: Int): ListNode = {
    var cur = head
    val left = mutable.Buffer.empty[Int]
    val right = mutable.Buffer.empty[Int]
    while (cur != null) {
      if (cur.x >= x) {
        right.append(cur.x)
      } else {
        left.append(cur.x)
      }
      cur = cur.next
    }
    (left ++ right).foldRight[ListNode](null)((x, next) => ListNode(x, next))
  }


  def main(args: Array[String]): Unit = {

    val head = ListNode(Array(1, 4, 3, 2, 5, 2))

    println(head)
    val x = 3

    PartitionList86(head, x)

  }


}
