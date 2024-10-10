package org.eterk.klearn.data_structure

import org.eterk.klearn.LeetCode

object GameOfLife239 extends LeetCode[Array[Array[Int]], Array[Array[Int]]] {

  override def no: Int = 289

  override def desc: String = "给定一个包含 m × n 个格子的面板，每一个格子都可以看成是一个细胞。每个细胞都具有一个初始状态： 1 即为 活细胞 （live），或 0 即为 死细胞 （dead）。每个细胞与其八个相邻位置（水平，垂直，对角线）的细胞都遵循以下四条生存定律：如果活细胞周围八个位置的活细胞数少于两个，则该位置活细胞死亡；如果活细胞周围八个位置有两个或三个活细胞，则该位置活细胞仍然存活；如果活细胞周围八个位置有超过三个活细胞，则该位置活细胞死亡；如果死细胞周围正好有三个活细胞，则该位置死细胞复活；下一个状态是通过将上述规则同时应用于当前状态下的每个细胞所形成的，其中细胞的出生和死亡是同时发生的。给你 m x n 网格面板 board 的当前状态，返回下一个状态。"

  override def domain(input: Array[Array[Int]]): Boolean = {
    val m = input.length
    val n = if (m > 0) input(0).length else 0
    1 <= m && m <= 25 && 1 <= n && n <= 25 && input.forall(row => row.length == n && row.forall(cell => cell == 0 || cell == 1))
  }

  override def stdIO: Seq[(Array[Array[Int]], Array[Array[Int]])] = Seq(
    ((Array(Array(0, 1, 0), Array(0, 0, 1), Array(1, 1, 1), Array(0, 0, 0))), Array(Array(0, 0, 0), Array(1, 0, 1), Array(0, 1, 1), Array(0, 1, 0))),
    ((Array(Array(1, 1), Array(1, 0))), Array(Array(1, 1), Array(1, 1)))
  )

  override def set: Map[String, Array[Array[Int]] => Array[Array[Int]]] = Map("矩阵" -> gameOfLife)

  def gameOfLife(board: Array[Array[Int]]): Array[Array[Int]] = {

    def turn(state: Int, num: Int): Int = {
      state match {
        case 0 if num == 3 => 1
        case 1 if num < 2 || num > 3 => 0
        case other => other
      }
    }

    def visit(i: Int, j: Int): Int = {
      scala.util.Try(board(i)(j)) match {
        case scala.util.Success(value) => value
        case scala.util.Failure(e) => 0
      }
    }

    def compute(i: Int, j: Int): Int = {
      val s = (s: Int) => Seq(s - 1, s, s + 1)

      s(i).flatMap(h => s(j).map(w => visit(h, w))).sum - visit(i, j)
    }

    val nums: Seq[Seq[(Int, Int)]] = board.indices.map(i => board.head.indices.map(j => board(i)(j) -> compute(i, j)))

    for (i <- board.indices) {
      for (j <- board.head.indices) {
        val (state, num) = nums(i)(j)
        board(i)(j) = turn(state, num)
      }
    }

    board
  }

}
