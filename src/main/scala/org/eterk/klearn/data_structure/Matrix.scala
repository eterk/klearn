package org.eterk.klearn.data_structure

trait Matrix {
  def apply(row: Int, col: Int): Int

  def update(row: Int, col: Int, value: Int): Unit
}

class ArrayMatrix(val rows: Int, val cols: Int) extends Matrix {
  private val data = Array.ofDim[Int](rows, cols)

  override def apply(row: Int, col: Int): Int = data(row)(col)

  override def update(row: Int, col: Int, value: Int): Unit = data(row)(col) = value
}

class Triple(val row: Int, val col: Int, var value: Int)

class TripleSeqMatrix(val rows: Int, val cols: Int) extends Matrix {
  private var data = Array[Triple]()

  override def apply(row: Int, col: Int): Int = {
    data.find(t => t.row == row && t.col == col).map(_.value).getOrElse(0)
  }

  override def update(row: Int, col: Int, value: Int): Unit = {
    data.find(t => t.row == row && t.col == col) match {
      case Some(triple) => triple.value = value
      case None => data :+= new Triple(row, col, value)
    }
  }
}

class OrthogonalNode(val row: Int, val col: Int, var value: Int) {
  var right: OrthogonalNode = _
  var down: OrthogonalNode = _
}

class OrthogonalListMatrix(val rows: Int, val cols: Int) extends Matrix {
  private val rowHeads = Array.fill[OrthogonalNode](rows)(new OrthogonalNode(0, 0, 0))
  private val colHeads = Array.fill[OrthogonalNode](cols)(new OrthogonalNode(0, 0, 0))

  override def apply(row: Int, col: Int): Int = {
    var node = rowHeads(row - 1).right
    while (node != null && node.col < col) {
      node = node.right
    }
    if (node != null && node.col == col) node.value else 0
  }

  override def update(row: Int, col: Int, value: Int): Unit = {
    var prevRowNode = rowHeads(row - 1)
    while (prevRowNode.right != null && prevRowNode.right.col < col) {
      prevRowNode = prevRowNode.right
    }
    var prevColNode = colHeads(col - 1)
    while (prevColNode.down != null && prevColNode.down.row < row) {
      prevColNode = prevColNode.down
    }
    if (prevRowNode.right != null && prevRowNode.right.col == col) {
      if (value == 0) {
        val node = prevRowNode.right
        prevRowNode.right = node.right
        prevColNode.down = node.down
      } else {
        prevRowNode.right.value = value
      }
    } else if (value != 0) {
      val node = new OrthogonalNode(row, col, value)
      node.right = prevRowNode.right
      prevRowNode.right = node
      node.down = prevColNode.down
      prevColNode.down = node
    }
  }
}
