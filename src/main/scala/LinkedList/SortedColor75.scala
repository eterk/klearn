package org.eter.klearn
package LinkedList

object SortedColor75 {
  def apply(nums: Array[Int]): Unit = {

    s1(nums)
  }

  def s1(nums: Array[Int]): Unit = {

    for (i <- nums.indices) {

      for (j <- ((i + 1) until nums.length)) {
        if (nums(j) < nums(i)) {
          nums(j) = nums(i) ^ nums(j)
          nums(i) = nums(i) ^ nums(j)
          nums(j) = nums(i) ^ nums(j)
        }

      }
    }
  }

  def s2(nums: Array[Int]): Unit = {
    var i = nums.length - 1
    while (i >= 0) {
      for (j <- (0 until i)) {
        if (nums(j) > nums(j + 1)) {
          nums(j + 1) = nums(j + 1) ^ nums(j)
          nums(j) = nums(j + 1) ^ nums(j)
          nums(j + 1) = nums(j + 1) ^ nums(j)
        }
      }
      i -= 1
    }
  }

  def s3(nums: Array[Int]) = {
    var i = nums.length - 1
    var tmp: Int = 0
    while (i >= 0) {
      for (j <- (0 until i)) {
        if (nums(j) > nums(j + 1)) {
          tmp = nums(j + 1)
          nums(j + 1) = nums(j)
          nums(j) = tmp
        }
      }
      i -= 1
    }
  }

  /**
   * 1.将待排序序列构造成一个大顶堆（升序大顶堆，降序小顶堆）
   * 2. 此时，整个序列的最大值就是堆顶的根节点
   * 3.将其与末尾元素进行交换，此时末尾就为最大值
   * 重复步骤1-3，直到堆中只剩下一个元素1
   *堆排序
   **/
  def s4(arr: Array[Int]): Array[Int] = {
    // 构建大顶堆
    def buildMaxHeap(arr: Array[Int], heapSize: Int): Unit = {
      // 从最后一个非叶子节点开始，自下而上，自右而左调整堆
      for (i <- heapSize / 2 - 1 to 0 by -1) {
        maxHeapify(arr, i, heapSize)
      }
    }

    // 调整堆
    def maxHeapify(arr: Array[Int], i: Int, heapSize: Int): Unit = {
      // 计算左右子节点的索引
      val l = i * 2 + 1
      val r = i * 2 + 2
      // 找出当前节点和左右子节点中最大的一个
      var largest = i
      if (l < heapSize && arr(l) > arr(largest)) {
        largest = l
      }
      if (r < heapSize && arr(r) > arr(largest)) {
        largest = r
      }
      // 如果最大的不是当前节点，就交换位置，并递归调整被交换的子树
      if (largest != i) {
        swap(arr, i, largest)
        maxHeapify(arr, largest, heapSize)
      }
    }

    // 交换元素
    def swap(arr: Array[Int], i: Int, j: Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    // 排序过程
    var heapSize = arr.length
    // 首先构建一个大顶堆，保证堆顶是最大值
    buildMaxHeap(arr, heapSize)
    // 然后将堆顶和堆尾交换，并缩小堆的大小，再调整堆，重复这个过程，直到只剩下一个元素
    for (i <- arr.length - 1 to 1 by -1) {
      swap(arr, 0, i)
      heapSize -= 1
      maxHeapify(arr, 0, heapSize)
    }

    // 返回结果
    arr
  }


  def main(args: Array[String]): Unit = {


    s4(Array(2, 3, 1, 43, 2, 1, 4, 1, 4))

  }

}
