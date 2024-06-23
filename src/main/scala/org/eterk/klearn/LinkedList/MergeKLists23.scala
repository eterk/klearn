package org.eterk.klearn.LinkedList

import scala.collection.mutable.ArrayBuffer

object MergeKLists23 {
  def apply(lists: Array[ListNode]): ListNode = {
    s1(lists)

  }

  def s1(lists: Array[ListNode]): ListNode = {

    if (lists.isEmpty) return null
    val container = lists.collect { case x if x != null => x }.toBuffer
    if (container.isEmpty) return null
    val dummy = ListNode(999, null)
    var cur = dummy
    var flag = true
    while (flag) {
      var minIndex = 0
      var min = Int.MaxValue
      //      println(dummy.next)
      for (index <- container.indices) {
        val current = container(index).x
        if (current < min) {
          min = current
          minIndex = index
        }
      }
      val minNow = container(minIndex)
      if (minNow.next == null) {
        container.remove(minIndex)
        if (container.isEmpty) {
          flag = false
        }
      } else {
        container.update(minIndex, minNow.next)
      }


      cur.next = minNow
      cur = cur.next
    }
    dummy.next
  }

  def s2(lists: Array[ListNode]): ListNode = {

    if (lists.isEmpty) return null
    val container = lists.collect { case x if x != null => x }.toBuffer
    if (container.isEmpty) return null
    val dummy = ListNode(999, null)
    var cur = dummy
    var flag = true
    while (flag) {
      var minIndex = 0
      var min = Int.MaxValue
      //      println(dummy.next)
      for (index <- container.indices) {
        val current = container(index).x
        if (current < min) {
          min = current
          minIndex = index
        }
      }
      val minNow = container(minIndex)
      if (minNow.next == null) {
        container.remove(minIndex)
        if (container.isEmpty) {
          flag = false
        }
      } else {
        container.update(minIndex, minNow.next)
      }


      cur.next = minNow
      cur = cur.next
    }
    dummy.next
  }


  def main(args: Array[String]): Unit = {


    def splitByN(a: Array[Int], k: Int): Array[Array[Int]] = {
      if (a.isEmpty) return Array.empty[Array[Int]]
      a.take(k) +: splitByN(a.drop(k), k)
    }

    val size = 3
    val arr = splitByN((0 to 12).toArray, 3)
    val res: Seq[Array[Int]] = {
      (0 until size).map(s => {
        val buffer = Array.empty[Int].toBuffer
        for (i <- arr.indices) {
          if (arr(i).isDefinedAt(s)) {
            buffer.append(arr(i)(s))
          }
        }
        buffer.toArray
      })

    }
    val v = res.map(x => ListNode(x)).toArray

    v.foreach(println)

    s1(v)


  }


}
