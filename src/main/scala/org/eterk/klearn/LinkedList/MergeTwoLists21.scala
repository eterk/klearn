package org.eterk.klearn.LinkedList

import scala.collection.mutable

object MergeTwoLists21 {


  def apply(list1: ListNode, list2: ListNode): ListNode = {

    s1(list1, list2)
  }

  /**
   * 这是最简洁的写法
   */
  def s0(l1: ListNode, l2: ListNode): ListNode = {
    if (l1 == null) return l2
    if (l2 == null) return l1
    if (l1.x < l2.x) {
      l1.next = s0(l1.next, l2)
      l1
    } else {
      l2.next = s0(l1, l2.next)
      l2
    }
  }

  def s1(list1: ListNode, list2: ListNode): ListNode = {
    if (list1 == null) return list2
    if (list2 == null) return list1

    def toList(x: ListNode): Array[Int] = {
      if (x.next == null) {
        Array(x.x)
      } else {
        x.x +: toList(x.next)
      }
    }

    val a1: Array[Int] = toList(list1)
    val a2: Array[Int] = toList(list2)

    val result = mergeTwoArray(a1, a2)

    result.foldRight[ListNode](null)((value, next) => ListNode(value, next))

  }


  private def mergeTwoArray(a1: Array[Int], a2: Array[Int]) = {

    var li = 0
    var ri = 0

    val buffer = mutable.Buffer[Int]()
    // 当 12 都超出范围停止
    while (!(li >= a1.length && ri >= a2.length)) {

      // 12 都没有超出范围
      if (li < a1.length && ri < a2.length) {
        if (a1(li) < a2(ri)) {
          buffer.append(a1(li))
          li += 1
        } else {
          buffer.append(a2(ri))
          ri += 1
        }
      }
      // 1超出范围
      if (li >= a1.length && ri < a2.length) {
        buffer.append(a2(ri))
        ri += 1
      }

      //2 超出范围
      if (ri >= a2.length && li < a1.length) {
        buffer.append(a1(li))
        li += 1
      }

    }
    buffer
  }

  def s2(l1: ListNode, l2: ListNode): ListNode = {
    val dummyHead = new ListNode(0)
    var p = dummyHead
    var p1 = l1
    var p2 = l2

    while (p1 != null && p2 != null) {
      if (p1.x <= p2.x) {
        p.next = p1
        p1 = p1.next
      } else {
        p.next = p2
        p2 = p2.next
      }
      p = p.next
    }

    if (p1 != null) {
      p.next = p1
    }

    if (p2 != null) {
      p.next = p2
    }

    dummyHead.next

  }


  def main(args: Array[String]): Unit = {

    val value = mergeTwoArray(Array(1, 2, 4), Array(1, 3, 4))
    //      mergeTwoArray(Array(1, 2, 9, 35, 621), Array(3, 4, 5, 9, 10))


    println(value.mkString(","))
  }


}
