class HIndex274 extends LeetCode[Array[Int], Int] {
  override def no: Int = 274

  override def domain(input: Array[Int]): Boolean = {
    input.length >= 1 && input.length <= 5000 && input.forall(c => c >= 0 && c <= 1000)
  }

  override def stdIO: Seq[(Array[Int], Int)] = Seq(
    (Array(3, 0, 6, 1, 5), 3),
    (Array(1, 3, 1), 1)
  )

  override def set: Map[String, Array[Int] => Int] = Map(
    "示例解法" -> (input => 0) // 这里只是一个示例，你可以替换为你自己的解法
  )
}

