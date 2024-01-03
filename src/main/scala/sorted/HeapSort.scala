package org.eter.klearn
package sorted

import scala.annotation.tailrec

object HeapSort {
  def heapSort(arr: Array[Int]): Array[Int] = {
    // 获取数组长度
    var len = arr.length

    // 构建大根堆
    buildMaxHeap(arr, len)

    // 将堆顶元素与末尾元素交换，然后重新调整大根堆
    for (i <- Range(len - 1, 0, -1)) {
      swap(arr, 0, i)
      len -= 1
      heapify(arr, 0, len)
    }

    // 返回排序后的数组
    arr
  }

  // 构建大根堆
  def buildMaxHeap(arr: Array[Int], len: Int): Unit = {
    for (i <- Range(len / 2, -1, -1)) {
      heapify(arr, i, len)
    }
  }

  // 调整大根堆
  def heapify(arr: Array[Int], i: Int, len: Int): Unit = {
    // 获取左右子节点的下标
    val left = 2 * i + 1
    val right = 2 * i + 2

    // 找出三者中的最大值的下标
    var largest = i
    if (left < len && arr(left) > arr(largest)) {
      largest = left
    }
    if (right < len && arr(right) > arr(largest)) {
      largest = right
    }

    // 如果最大值不是父节点，则交换并继续调整
    if (largest != i) {
      swap(arr, i, largest)
      heapify(arr, largest, len)
    }
  }

  // 元素交换函数
  def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }


  def main(args: Array[String]): Unit = {
    heapSort(Array(8, 7, 6, 5, 4, 3, 2, 1))
  }

}
