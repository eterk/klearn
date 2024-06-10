package org.eterk.klearn.untyped

object ValidSudoku36 extends LeetCode[Array[Array[Char]], Boolean] {
  override def no: Int = 36

  override def desc: String = "请你判断一个 9 x 9 的数独是否有效。只需要 根据以下规则 ，验证已经填入的数字是否有效即可。数字 1-9 在每一行只能出现一次。数字 1-9 在每一列只能出现一次。数字 1-9 在每一个以粗实线分隔的 3x3 宫内只能出现一次。"

  override def domain(input: Array[Array[Char]]): Boolean = {
    input.length == 9 && input.forall(row => row.length == 9 && row.forall(ch => ch.isDigit || ch == '.'))
  }

  override def stdIO: Seq[(Array[Array[Char]], Boolean)] = Seq(
    (Array(
      Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
    ), true),
    (Array(
      Array('8', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
    ), false)
  )

  import scala.util.control.Breaks._

  def splitByN(seq: Array[Int], step: Int): Array[Array[Int]] = {
    if (seq.isEmpty) return Array.empty[Array[Int]]
    seq.take(step) +: splitByN(seq.slice(step, seq.length), step)
  }

  def isValidSudoku(board: Array[Array[Char]]): Boolean = {

    val isLegal = (numSrc: Array[Char]) => {
      val nums = numSrc.filterNot('.' == _)
      nums.length == nums.distinct.length
    }
    var res = board.forall(isLegal) &&
      board.transpose.forall(isLegal)

    val index = splitByN((0 until 9).toArray, 3)

    breakable {
      for (cols <- index) {
        for (rows <- index) {
          if (!isLegal(cols.flatMap(c => rows.map(r => board(c)(r))))) {
            res = false
            break
          }
        }
      }

    }


    res
  }


  override def set: Map[String, Array[Array[Char]] => Boolean] = {
    Map("双指针" -> isValidSudoku)

  }
}
