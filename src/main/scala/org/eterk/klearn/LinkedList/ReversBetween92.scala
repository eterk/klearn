package org.eterk.klearn.LinkedList

object ReversBetween92 {

  def apply(head: ListNode, left: Int, right: Int): ListNode = {

    s1(head, left, right)
  }

  /**
   * 理解错了题意
   *
   * @return
   */
  @Deprecated
  def s1(head: ListNode, left: Int, right: Int): ListNode = {

    if (left == right || head == null) return head

    var ci = 1
    var lVal: Int = 0
    var rVal: Int = 0
    var curr = head
    while (curr != null) {
      if (left == ci) lVal = curr.x
      if (right == ci) {
        rVal = curr.x
        curr.x = lVal
      }
      curr = curr.next
      ci += 1
    }
    ci = 1
    curr = head
    while (ci <= left && curr != null) {
      if (ci == left) {
        curr.x = rVal
      }
      curr = curr.next
      ci += 1
    }
    head
  }

  /**
   *
   * 使用暴力解法解决这道题
   * Runtime 539 ms Beats 66.67% Memory 55.2 MB Beats 77.78%
   */
  def s2(head: ListNode, left: Int, right: Int): ListNode = {
    if (left == right || head == null) return head
    var i = 1
    val l = collection.mutable.ArrayBuffer.empty[Int]
    val m = collection.mutable.ArrayBuffer.empty[Int]
    val r = collection.mutable.ArrayBuffer.empty[Int]

    var curr = head
    while (curr != null) {
      if (i < left) {
        l.append(curr.x)
      } else if (i >= left && i <= right) {
        m.append(curr.x)
      } else {
        r.append(curr.x)
      }
      i += 1
      curr = curr.next
    }
    (l ++ m.reverse ++ r).foldRight[ListNode](null)((x, next) => ListNode(x, next))

  }

  /**
   *
   * Runtime 492 ms Beats 100% Memory 55 MB Beats 77.78%
   */
  def s3(head: ListNode, left: Int, right: Int): ListNode = {
    if (head == null || left == right) return head // 如果链表为空或者只有一个节点，直接返回
    val dummy = new ListNode(0) // 创建一个虚拟头节点，方便操作
    dummy.next = head // 将虚拟头节点指向原链表的头节点
    var pre: ListNode = dummy // 定义一个前驱节点，用于记录反转区间的前一个节点
    for (_ <- 1 until left) pre = pre.next // 将前驱节点移动到反转区间的前一个位置
    val start: ListNode = pre.next // 定义一个开始节点，用于记录反转区间的第一个节点
    var andThen: ListNode = start.next // 定义一个后继节点，用于记录开始节点的下一个节点

    for (_ <- 0 until right - left) { // 循环right-left次，每次将后继节点插入到前驱节点和开始节点之间，实现局部反转
      start.next = andThen.next // 将开始节点指向后继节点的下一个节点，断开和后继节点的连接
      andThen.next = pre.next // 将后继节点指向前驱节点的下一个节点，即开始节点
      pre.next = andThen // 将前驱节点指向后继节点，完成插入操作
      andThen = start.next // 更新后继节点为开始节点的下一个节点，准备下一次循环
    }

    dummy.next // 返回虚拟头结点的下一个结点，即新链表的头结点

  }

  def main(args: Array[String]): Unit = {

    val head=ListNode(Array(1,2,3,4,5))
    s3(head,2,4)


  }

}
