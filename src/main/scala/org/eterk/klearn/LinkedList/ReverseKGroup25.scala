package org.eterk.klearn.LinkedList

import scala.collection.mutable

object ReverseKGroup25 {

  def apply(head: ListNode, k: Int): ListNode = {

    s1(head, k)

  }

  def s1(head: ListNode, k: Int): ListNode = {
    val stack = new mutable.Stack[ListNode]()
    var cur: ListNode = head
    var res: ListNode = null
    var k2: ListNode = null
    var flag = 0

    while (cur != null) {
      if (cur.next == null && flag == 0) {
        flag = 1
        cur.next = ListNode(999, null)
      }


      // 将该区段的倒置
      if (stack.size == k || cur.next == null) {
        if (stack.nonEmpty) {
          var k1 = ListNode(stack.pop().x, null)

          if (k2 != null) k2.next = k1 //将后面的node 链接到前面node 上

          if (res == null) res = k1 //初始化 结果链表的头

          while (stack.nonEmpty) {
            k2 = ListNode(stack.pop().x, null)

            k1.next = k2
            k1 = k2
          }
        }
      }

      stack.push(cur) // 将当前元素推入栈中
      cur = cur.next // 遍历到下个元素
    }

    res
  }


  def s2(head: ListNode, k: Int): ListNode = {

    var curr = head
    var res: ListNode = null
    var k2: ListNode = null
    var needOver = true
    var keepGoing = true

    val stack = collection.mutable.Stack.empty[ListNode]
    while (keepGoing) {

      // 将该区段的倒置
      if (stack.size == k || (curr.next == null && (!needOver))) {
        if (stack.nonEmpty) {
          var k1 = ListNode(stack.pop().x, null)

          if (k2 != null) k2.next = k1 //将后面的node 链接到前面node 上

          if (res == null) res = k1 //初始化 结果链表的头

          while (stack.nonEmpty) {
            k2 = ListNode(stack.pop().x, null)

            k1.next = k2
            k1 = k2
          }
        }
      }

      if (curr.next == null) {
        if (needOver) {
          curr.next = ListNode(-999, null)
          needOver = false
        } else {
          keepGoing = false
        }
      }
      stack.push(curr) // 将当前元素推入栈中
      curr = curr.next // 遍历到下个元素
    }
    res
  }


  /**
   * 写了好久写了一坨大便
   */
  def s3(head: ListNode, k: Int): ListNode = {
    if(k==1) return head
    var curr = head
    var res: ListNode = null
    var k2: ListNode = null
    var needOver = true
    var keepGoing = true


    val stack = collection.mutable.Stack.empty[ListNode]
    while (keepGoing) {
      // 将该区段的倒置
      if (stack.size == k || (curr.next == null && (!needOver))) {

        if (stack.size == k) {

          var k1 = ListNode(stack.pop().x, null)

          if (k2 != null) k2.next = k1 //将后面的node 链接到前面node 上

          if (res == null) res = k1 //初始化 结果链表的头

          while (stack.nonEmpty) {
            k2 = ListNode(stack.pop().x, null)

            k1.next = k2
            k1 = k2
          }
        } else if (stack.size > 0 && stack.size < k) {


          var i = 0
          var last: ListNode = null
          while (stack.nonEmpty) {
            last = stack.pop()
            i += 1
            if (i == 1) {
              last.next = null
            }
          }

          if (res == null) res = last //初始化 结果链表的头
          if (k2 != null) k2.next = last //将后面的node 链接到前面node 上

        }
      }

      if (curr.next == null) {
        if (needOver) {
          curr.next = ListNode(-999, null)
          needOver = false
        } else {
          keepGoing = false
        }
      }
      stack.push(curr) // 将当前元素推入栈中
      curr = curr.next // 遍历到下个元素
    }
    res
  }

  def s4(head: ListNode, k: Int): ListNode = {
    if (k < 2) return head // edge case

    val q = mutable.Queue.empty[mutable.Stack[Int]]
    var s = mutable.Stack.empty[Int] // max k length

    // build reverse order
    var currentNode = head
    while(currentNode != null) {
      s.push(currentNode.x)
      currentNode = currentNode.next
      if (s.length == k) {
        q.enqueue(s)
        s = mutable.Stack.empty[Int]
      }
    }

    // rebuild with reverse Order
    currentNode = head
    s = mutable.Stack.empty[Int]
    while(q.nonEmpty || s.nonEmpty) {
      if (s.isEmpty) s = q.dequeue()
      currentNode.x = s.pop()
      currentNode = currentNode.next
    }

    head
  }


  def main(args: Array[String]): Unit = {

    val k = 1
    val max = 12
    (0 to max).foreach(i => {
      val input = ListNode((0 to i).toArray)
      val r = s4(input, k)
      //      println(r.size+" "+input.size)
      println(i + ":   " + r)
    })

  }


}
