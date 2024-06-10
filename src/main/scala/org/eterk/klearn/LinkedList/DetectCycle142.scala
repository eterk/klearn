package org.eterk.klearn.LinkedList

object DetectCycle142 {

  def apply(head: ListNode): ListNode = {

    s1(head)
  }

  /**
   *  Runtime 569 ms Beats 86.5% Memory 54.1 MB Beats 39.53%
   * @param head
   * @return
   */
  def s1(head: ListNode): ListNode = {

    val container = collection.mutable.Map[ListNode, Boolean]()

    var c = head
    while (c != null) {
      if (container.isDefinedAt(c)) return c
      container.getOrElseUpdate(c, false)
      c = c.next
    }
    return null
  }


  def s2(head:ListNode):ListNode={

    if (head == null || head.next == null) return null // 链表为空或只有一个节点时无环
    var fast = head // 快指针
    var slow = head // 慢指针
    var start = head // 新指针

    while (fast != null && fast.next != null) { // 遍历链表直到快指针为空或到达尾部
      fast = fast.next.next // 快指针每次走两步
      slow = slow.next // 慢指针每次走一步
      if (fast == slow) { // 如果快慢指针相遇了，则说明有环
        while (start != slow) { // 让新指针和慢指针同时走，直到再次相遇为止
          start = start.next // 新指针每次走一步
          slow = slow.next // 慢指针每次走一步
        }
        return start // 返回相遇点即为环的入口节点
      }
    }
    return null // 如果快慢指针没有相遇，则说明无环，返回null

  }



}
