package org.eter.klearn
package LinkedList

import scala.collection.mutable.ArrayBuffer

object CopyRandomList138 {

  class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
  }

  def apply(head: Node): Node = {
    head
  }

  /**
   * Runtime607 ms Beats 12.50% Memory55 MB Beats 6.25%
   */
  def s1(head: Node): Node = {

    if (head == null) return null
    val map = collection.mutable.Map[Node, Int]()

    var i = 0
    var cur = head
    while (cur != null) {
      map.getOrElseUpdate(cur, i)
      cur = cur.next
      i += 1
    }

    val x = collection.mutable.Map[Int, (Node, Int)]()
    map.map {
      case (k, v) =>
        if (k.random == null) {
          (k.value, v, -1)
        } else {
          (k.value, v, map(k.random))
        }
    }.toSeq
      .sortBy(_._2)
      .foldRight[Node](null)((v: (Int, Int, Int), n: Node) => {
        val (value, index, randomIndex) = v
        val current = new Node(value)
        current.next = n
        x.update(index, (current, randomIndex))
        current
      })

    for (i <- (0 until i)) {
      if (x(i)._2 != -1) {
        x(i)._1.random = x(x(i)._2)._1
      }
    }

    x(0)._1
  }

  /**
   * 遍历原始链表，对每个节点创建一个复制节点，并将其插入到原始节点的后面。例如，如果原始链表是A->B->C，那么插入后的链表是A->A’->B->B’->C->C’。
   * 再次遍历链表，对每个复制节点，将其随机指针指向原始节点的随机指针所指向的节点的下一个节点。例如，如果A.random = C，那么A’.random = C.next = C’。
   * 最后，将链表拆分为两个链表，一个是原始链表，一个是复制链表。注意要恢复原始链表的结构。
   */
  def s2(head: Node): Node = {
    // 如果链表为空，直接返回null
    if (head == null) return null

    // 第一次遍历，插入复制节点
    var cur = head // 定义一个指针指向当前节点
    while (cur != null) { // 当指针不为空时循环
      val copy = new Node(cur.value) // 创建一个复制节点，值和当前节点相同
      copy.next = cur.next // 将复制节点的next指针指向当前节点的next指针所指向的节点
      cur.next = copy // 将当前节点的next指针指向复制节点
      cur = copy.next // 将当前指针移动到下一个原始节点（跳过复制节点）
    }

    // 第二次遍历，设置随机指针
    cur = head // 将当前指针重置为头节点
    while (cur != null) { // 当指针不为空时循环
      val copy = cur.next // 获取当前节点对应的复制节点（在其后面）
      if (cur.random != null) { // 如果当前节点有随机指针，那么设置复制节点的随机指针为原始随机指针所指向的下一个（即对应的复制）
        copy.random = cur.random.next
      }
      cur = copy.next // 将当前指针移动到下一个原始节点（跳过复制）
    }

    // 第三次遍历，拆分两个链表并恢复原始链表结构
    val dummyHead = new Node(0) // 创建一个虚拟头结点用于存储复制链表头结点（方便返回）
    var copyCur = dummyHead // 定义一个新的指针用于遍历复制链表并连接其各个结点
    cur = head // 将当前指针重置为头结点
    while (cur != null) { // 当前不为空时循环
      val nextOriginalNode= cur.next.next;  	// 获取下一个原始结点（跳过其后面紧挨着的copy node）
      val copyNode= cur.next;	// 获取copy node
      copyCur.next=copyNode;	// 连接copy node
      copyCur=copyCur.next;	// 移动copyCur 指向最新添加进来的copy node

      // 恢复原始链表结构
      cur.next=nextOriginalNode;
      cur=nextOriginalNode;
    }
    return dummyHead.next // 返回复制链表的头结点
  }

}
