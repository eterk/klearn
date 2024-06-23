package org.eterk.klearn

object Utils {
  def splitByN(a: Array[Int], k: Int): Array[Array[Int]] = {
    if (a.isEmpty) return Array.empty[Array[Int]]
    a.take(k) +: splitByN(a.drop(k), k)
  }


}
