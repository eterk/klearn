package org.eter.klearn
package sorted

object MergeSort {

  def mergeSort(list: List[Int]): List[Int] = {
    val n = list.length / 2
    if (n == 0) list
    else {
      val (left, right) = list.splitAt(n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (x :: xs, y :: ys) =>
        if (x < y) x :: merge(xs, right)
        else y :: merge(left, ys)
    }

}
