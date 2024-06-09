object SetMatrixZeroes73 extends LeetCode[Array[Array[Int]], Array[Array[Int]]] {
  override def no: Int = 73

  override def desc: String = "给定一个 m x n 的矩阵，如果一个元素为 0 ，则将其所在行和列的所有元素都设为 0 。请使用 原地 算法。"

  override def domain(input: Array[Array[Int]]): Boolean = {
    val m = input.length
    val n = input(0).length
    1 <= m && m <= 200 && 1 <= n && n <= 200 && input.forall(row => row.forall(num => -231 <= num && num <= 231 - 1))
  }

  override def stdIO: Seq[(Array[Array[Int]], Array[Array[Int]])] = Seq(
    (Array(
      Array(1, 1, 1),
      Array(1, 0, 1),
      Array(1, 1, 1)
    ), Array(
      Array(1, 0, 1),
      Array(0, 0, 0),
      Array(1, 0, 1)
    )),
    (Array(
      Array(0, 1, 2, 0),
      Array(3, 4, 5, 2),
      Array(1, 3, 1, 5)
    ), Array(
      Array(0, 0, 0, 0),
      Array(0, 4, 5, 0),
      Array(0, 3, 1, 0)
    ))
  )

  override def set: Map[String, Array[Array[Int]] => Array[Array[Int]]] = Map("矩阵" -> setZeroes)

  def setZeroes(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    var rows = collection.mutable.Set.empty[Int]
    var cols = collection.mutable.Set.empty[Int]

    for (i <- matrix.indices) {
      for (j <- matrix.head.indices) {
        if (matrix(i)(j) == 0) {
          rows += i
          cols += j
        }
      }
    }
    rows.foreach(i => matrix(i).indices.foreach(j => matrix(i)(j) = 0))
    matrix.indices.foreach(i => cols.foreach(j => matrix(i)(j) = 0))
    matrix
  }
}
