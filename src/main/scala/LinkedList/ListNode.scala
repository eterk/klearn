package org.eter.klearn
package LinkedList


class ListNode(var _x: Int = 0) {
  var next: ListNode = _
  var x: Int = _x

  override def toString: String = {
    if (next != null) {
      x + "," + next.toString
    } else {
      x.toString
    }
  }
}

object ListNode {
  def apply(_x: Int, _next: ListNode): ListNode = {
    val v = new ListNode(_x)
    v.next = _next
    v
  }

  def apply(arr: Array[Int]): ListNode = {
    arr.foldRight[ListNode](null)((current, next) => {
      val y = new ListNode(current)
      y.next = next
      y
    })
  }

  def apply(str: String): ListNode = {
    apply(str.split(",").map(_.toInt))
  }
}
