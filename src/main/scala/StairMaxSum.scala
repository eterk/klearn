package org.eter.klearn

object StairMaxSum {

  /*  7
    3 8
    8 1 0
    2 7 4 4
    4 5 2 6 5
  从上到下选择一条路，使得经过的数字之和最大。

  路径上的每一步只能往左下或者右下走。

    */
  def exampleInput(seq: Seq[Int]): Seq[Seq[Int]] = {

    def split(seq: Seq[Int], nums: Int): Seq[Seq[Int]] = {
      if (seq.isEmpty) return Nil
      val (current, next) = seq.splitAt(nums)
      current +: split(next, nums + 1)
    }

    split(seq, 1)

  }

  def dynamicProgram(context: Seq[Seq[Int]]): Int = {
    val buffer = context.map(seq => seq.toBuffer)
    val iIndex = (0 until buffer.size - 1).reverse
    for (i <- iIndex) {
      for (j <- 0 to i) {
        buffer(i)(j) = Math.max(buffer(i + 1)(j), buffer(i + 1)(j + 1)) + buffer(i)(j)
      }
    }
    buffer.head.head
  }


  def main(args: Array[String]): Unit = {
    val input =
      exampleInput(Seq(7, 3, 8, 8, 1, 0, 2, 7, 4, 4, 4, 5, 2, 6, 5))

    //    input.foreach(x => println(x.mkString(",")))
    dynamicProgram(input)

  }

}
