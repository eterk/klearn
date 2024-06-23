package org.eterk.klearn.data_structure

import org.eterk.klearn.{LeetCode, Utils}


object SpiralMatrix54 extends LeetCode[Array[Array[Int]], List[Int]] {

  override def no: Int = 54

  override def desc: String = "给你一个 m 行 n 列的矩阵 matrix ，请按照 顺时针螺旋顺序 ，返回矩阵中的所有元素。"

  override def domain(input: Array[Array[Int]]): Boolean = {
    val m = input.length
    val n = if (m > 0) input(0).length else 0
    1 <= m && m <= 10 && 1 <= n && n <= 10 && input.forall(row => row.length == n && row.forall(cell => -100 <= cell && cell <= 100))
  }

  override def stdIO: Seq[(Array[Array[Int]], List[Int])] = {
    Utils.splitByN((1 to 16).toArray, 4)

    Seq(
      (Array(Array(1)), List(1)),
      (Array(Array()), List()),
      (Array(Array(3), Array(2)), List(3, 2)),
      (Utils.splitByN((1 to 16).toArray, 4), List(1, 2, 3, 4, 8, 12, 16, 15, 14, 13, 9, 5, 6, 7, 11, 10)),
      ((Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))), List(1, 2, 3, 6, 9, 8, 7, 4, 5)),
      (Utils.splitByN((1 to 12).toArray, 4), List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7))
    )
  }

  override def set: Map[String, Array[Array[Int]] => List[Int]] = Map("矩阵" -> spiralOrder)
//  "ai-opt" -> spiralOrder1

  import scala.annotation.tailrec

  //todo 做的时候比较慢,思路不复杂,但是细节出了许多问题
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {

    val orientSeq = Seq((0, 1), (1, 0), (0, -1), (-1, 0))
    var orient = 0
    val next = (loc: (Int, Int)) => {
      val (i, j) = orientSeq(orient)
      (loc._1 + i, loc._2 + j)
    }
    val res = collection.mutable.Buffer.empty[Int]
    val size = matrix.length * matrix.head.length

    @tailrec
    def rec(loc: (Int, Int), zero: (Int, Int), dim: (Int, Int)): Unit = {

      val dimIllegal = dim._1 < 1 || dim._2 < 1
      val locIllegal = loc._1 > matrix.length - 1 || loc._2 > matrix.head.length - 1 || loc._1 < 0 || loc._2 < 0
      val over = res.size == size

      if (dimIllegal || over || locIllegal) return;

      res.append(matrix(loc._1)(loc._2))
      var newZero = zero
      var newDim = dim


      dim match {
        case (1, n) => orient = 0
        case (m, 1) => orient = 1
        case (m, n) =>
          val maxM = zero._1 + dim._1 - 1
          val maxN = zero._2 + dim._2 - 1

          val topLeft = zero
          val topRight = (zero._1, maxN)
          val bottomRight = (maxM, maxN)
          val bottomLeft = (maxM, zero._2)
          val isLast = loc == (zero._1 + 1, zero._2)
          if (isLast) {
            newZero = (zero._1 + 1, zero._2 + 1)
            newDim = (dim._1 - 2, dim._2 - 2)
            orient = 0
          }
          val index = Seq(topLeft, topRight, bottomRight, bottomLeft).map(_ == loc).indexOf(true)
          if (index != -1) {
            orient = index
          }

      }


      rec(next(loc), newZero, newDim)


    }

    rec((0, 0), (0, 0), (matrix.length, matrix.head.length))

    res.toList

  }
  import scala.annotation.tailrec

  /**
   * ai 给我的简洁版本,有bug
   * //todo 帮AI改bug
   */
  def spiralOrder1(matrix: Array[Array[Int]]): List[Int] = {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
    val m = matrix.length
    val n = matrix.head.length
    val visited = Array.ofDim[Boolean](m, n)
    val res = collection.mutable.Buffer.empty[Int]


    @tailrec
    def traverse(x: Int, y: Int, dx: Int, dy: Int): Unit = {
      if (res.size == m * n) return

      if (x >= 0 && x < m && y >= 0 && y < n && !visited(x)(y)) {
        res += matrix(x)(y)
        visited(x)(y) = true
        traverse(x + dx, y + dy, dx, dy)
      } else {
        val newDirection = directions((directions.indexOf((dx, dy)) + 1) % directions.length)
        traverse(x + newDirection._1, y + newDirection._2, newDirection._1, newDirection._2)
      }
    }

    traverse(0, 0, 0, 1)
    res.toList
  }


}
