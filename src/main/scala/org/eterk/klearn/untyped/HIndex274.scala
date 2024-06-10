package org.eterk.klearn.untyped


object HIndex274 extends LeetCode[Array[Int], Int] {
  override def no: Int = 274

  override def domain(input: Array[Int]): Boolean = {
    input.length >= 1 && input.length <= 5000 && input.forall(c => c >= 0 && c <= 1000)
  }

  override def stdIO: Seq[(Array[Int], Int)] = Seq(
    (Array(3, 0, 6, 1, 5), 3),
    (Array(1, 3, 1), 1),
    (Array(100), 1),
    (Array(11, 15), 2),
    (Array(1, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), 2)
  )

  override def set: Map[String, Array[Int] => Int] = Map(
    "示例解法" -> s1 // 这里只是一个示例，你可以替换为你自己的解法
  )

  /**
   * left: 剩余数目
   * current:当前篇数
   * h:历史最大h 数目
   */
  def s1(citations: Array[Int]): Int = {
    val sorted = citations.sorted.iterator
    var i = 0
    var h = 0
    while (sorted.hasNext) {
      val current = sorted.next()
      val left = citations.length - i

      if (current <= left) {
        h = current
      } else {
        if (h < left) {
          h = left
        } else {
          h
        }
      }


      i += 1
    }

    h
  }
}

