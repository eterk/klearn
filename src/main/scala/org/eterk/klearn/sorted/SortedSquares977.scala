package org.eter.klearn
package sorted

object SortedSquares977 {
  def apply(nums: Array[Int]): Array[Int] = {
    s1(nums)
  }

  def s1(A: Array[Int]): Array[Int] = {

    val res = new Array[Int](A.length)
    //pointers for left and right
    var lo = 0
    val maxIndex = A.length - 1
    var hi = maxIndex
    //iterate from n to 0
    for (i <- maxIndex to 0 by -1) { //check if abs left is less than or equal to abs right
      if (Math.abs(A(lo)) >= Math.abs(A(hi))) { //add left squared to result array
        res(i) = A(lo) * A(lo)
        //increment left pointer
        lo += 1
      }
      else { //add right squared to result array
        res(i) = A(hi) * A(hi)
        //decrement right pointer
        hi -= 1
      }
      println(res.mkString(","))
    }
    res

  }

  def main(args: Array[String]): Unit = {


    s1(Array(1, 3, 5, 1, -1, -5, -9))


  }
}
