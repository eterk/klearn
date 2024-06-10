package org.eterk.klearn.LinkedList

/**
 * https://leetcode.com/problems/intersection-of-two-linked-lists/
 */
object GetIntersectionNode160 {


  def apply(headA: ListNode, headB: ListNode): ListNode = {

    s1(headA, headB)
  }


  /**
   * 我自己写的垃圾解决方案 没有搞定
   * 我的方案只能保证值相等，没法保证内存地址相同
   */
  def s1(headA: ListNode, headB: ListNode): ListNode = {
    //    val a=Array[Int]()
    val a = collection.mutable.Stack[ListNode]()
    val b = collection.mutable.Stack[ListNode]()
    var ai = headA
    var bi = headB
    while (ai.next != null) {
      a.push(ai)
      ai = ai.next
    }
    while (bi.next != null) {
      b.push(bi)
      bi = bi.next
    }
    a.push(ai)
    b.push(bi)
    var common: ListNode = null
    var flag = true
    while (flag && a.nonEmpty && b.nonEmpty) {
      ai = a.pop()
      bi = b.pop()
      if (ai == bi) {
        common = ai
      } else {
        flag = false
      }

    }


    common


  }

  /**
   *
   * 先把AB按照胃部对齐裁剪成同样长度后，开始直接比较，相等则返回，否则返回null
   */
  def s3(headA: ListNode, headB: ListNode): ListNode = {
    var lenA = 0
    var lenB = 0
    var currA = headA
    var currB = headB

    // find the lengths of both lists
    while (currA != null) {
      lenA += 1
      currA = currA.next
    }

    while (currB != null) {
      lenB += 1
      currB = currB.next
    }

    // reset the pointers to the heads of both lists
    currA = headA
    currB = headB

    // move the pointer of the longer list ahead by the difference in lengths
    if (lenA > lenB) {
      for (_ <- 0 until (lenA - lenB)) {
        currA = currA.next
      }
    } else if (lenB > lenA) {
      for (_ <- 0 until (lenB - lenA)) {
        currB = currB.next
      }
    }

    // compare the nodes until they match or reach null
    while (currA != null && currB != null) {
      if (currA == currB) return currA // found intersection node

      currA = currA.next
      currB = currB.next
    }

    return null // no intersection node found

  }

  /**
   * The second one uses a clever trick to make both pointers traverse equal distances by switching to the other
   * list’s head when they reach null, then compares them until they match or both become null
   *
   */
  def s2(head1: ListNode, head2: ListNode): ListNode = {

    var p1 = head1;
    var p2 = head2;

    //  任意一个为null 那么返回值肯定是null
    if (p1 == null || p2 == null)
      return null;

    while (p1 != p2) {
      //  if either pointer hits the end, switch head and continue the second traversal,
      //  if not hit the end, just move on to next
      p1 = if (p1 == null) head2 else p1.next;
      p2 = if (p2 == null) head1 else p2.next;

    }

    return p1;
  }

  /**
   *
   * @param args 4,1,8,4,5  5,6,1,8,4,5
   */
  def main(args: Array[String]): Unit = {
    val Array(a, b) = args

    println(GetIntersectionNode160(ListNode(a), ListNode(b)))


  }


}
