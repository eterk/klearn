package org.eterk.klearn.interview

import scala.io.StdIn


object HuaWeiOD1 {

  def main(args: Array[String]): Unit = {

    val input1 =
      Seq(
        "3",
        "head add 1",
        "remove",
        "tail add 2",
        "head add 3",
        "remove",
        "remove"
      )
    val input =
      Seq(
        "4",
        "head add 1",
        "remove",
        "tail add 2",
        "head add 3",
        "head add 4",
        "remove",
        "remove",
        "remove"
      )

    var seq: Array[Int] = Array.empty
    var count = 0
    var nums: Seq[Int] = Seq.empty
    var expectInt = 1

    def explain(str: String): Unit = {
      str match {
        case "remove" =>
          if (seq.head != expectInt) {
            // 不合适的地方需要调整seq
            val needIndex = seq.indexOf(expectInt)
            seq(needIndex) = seq.head
            count += 1
          }
          expectInt += 1
          seq = seq.tail
        case headAdd if headAdd.startsWith("head add") =>
          val num = headAdd.split(" ").last.toInt

          seq = num.toInt +: seq
          nums = nums.tail
        case tailAdd if tailAdd.startsWith("tail add") =>
          val num = tailAdd.split(" ").last.toInt
          seq = seq :+ num
          nums = nums.tail
        case num => nums = (1 to num.toInt)
      }

    }

    input.foreach(str => explain(str))
    println(nums)
    println(seq.toSeq)
    println(count)


  }


}

object HuaWeiOD {


  def isSame(s1: String, s2: String): Int = {
    var i = 0

    var allRight = false
    while (!allRight && i <= s2.length - s1.length) {
      var j = 0
      while (j < s1.length) {
        if (s1(j) == s2(i)) {
          if (j == s1.length - 1) {
            allRight = true
          }

          j += 1
          i += 1
        } else {
          j = s1.length
        }
      }

      i += 1
    }

    if (allRight) {
      println(i)
      i - s1.length - 1
    } else {
      -1
    }

  }

  def index(s1: String, s2: String, k: Int): Int = {
    if (k + s1.length > s2.length) return -1

    val startIndex = isSame(s1, s2)
    println(startIndex)
    if (startIndex < 0) {
      -1
    } else {
      math.max(0, startIndex - k)
    }


  }

  def index1(s1: String, s2: String, k: Int): Int = {
    if (k + s1.length > s2.length) return -1

    val startIndex =
      (0 to s2.length - s1.length).find(i => {
        println(s2.substring(i, i + s1.length))
        s1 == s2.substring(i, i + s1.length)
      })


    if (startIndex.isEmpty) {
      -1
    } else {
      val index = startIndex.get
      math.max(0, index - k)
    }
  }

  def main(args: Array[String]): Unit = {
    //    println(isSame("ab", "abcd"))
    //    println(isSame("ab", "aaab"))
    //    println(isSame("ab", "aaababab"))
    println(isSame("abc", "aacababcab"))

    //    println(index("ab", "aabcd", 1))
    //    println(index("abc", "dfs", 10))
    //    println(index("abc", "df", 2))
    //    println(index("abc", "dffabc", 2))
    //    println(index("abc", "abcdddd", 2))
    //    println(index("abc", "asdasdsaabcdddd", 2))
    //    println(index("abc", "abcsdsaabcdddd", 2))

  }
}


object SheepWolf {

  def main(args: Array[String]): Unit = {
    println(transfer(3, 5, 3))
    println(transfer(0, 5, 3))
    println(transfer(0, 5, 5))
    println(transfer(5, 0, 5))



  }

  def transfer(wolf: Int, sheep: Int, num: Int) = {

    val safe = (num: (Int, Int)) => {
      if (num._2 != 0) {
        num._1 < num._2
      } else {
        true
      }

    }

    val result = collection.mutable.Buffer.empty[Seq[(Int, Int)]]

    def boat(a: (Int, Int), b: (Int, Int), cSeq: Seq[(Int, Int)]): Unit = {


      if (a._1 == 0 && a._2 == 0) {
        result.append(cSeq)
        return
      }

      for (i <- 0 to num) { // 狼数量
        for (j <- 0 to num - i) { // 羊数量
          if (!(i == 0 && j == 0) && (i <= a._1 && j <= a._2)) {
            val c = (i, j)
            val newA = (a._1 - i, a._2 - j)
            val newB = (b._1 + i, b._2 + j)
            if (safe(newA) && safe(newB)) {
              boat(newA, newB, cSeq :+ c)
            }
          }
        }
      }

    }

    boat((wolf, sheep), (0, 0), Seq.empty)

    val re =
      result.map(_.length)

    if(re.isEmpty){
      -1

  }else{
      re.min
    }

  }


}


import scala.io._
import java.io._


object Main {
  def main(args :Array[String]): Unit = {

    try {
      val input =StdIn.readLine().split(" ").map(_.toInt)

      val res=transfer(input(1),input(0),input(2))
      println(res)

    } catch {
      case e: Exception => {


      }
    }
  }

  def transfer(wolf: Int, sheep: Int, num: Int) = {

    val safe = (num: (Int, Int)) => {
      if (num._2 != 0) {
        num._1 <= num._2
      } else {
        true
      }

    }

    val result = collection.mutable.Buffer.empty[Seq[(Int, Int)]]

    def boat(a: (Int, Int), b: (Int, Int), cSeq: Seq[(Int, Int)]): Unit = {


      if (a._1 == 0 && a._2 == 0) {
        result.append(cSeq)
        return
      }

      for (i <- 0 to num) { // 狼数量
        for (j <- 0 to num - i) { // 羊数量
          if (!(i == 0 && j == 0) && (i <= a._1 && j <= a._2)) {
            val c = (i, j)
            val newA = (a._1 - i, a._2 - j)
            val newB = (b._1 + i, b._2 + j)
            if (safe(newA) && safe(newB)) {
              boat(newA, newB, cSeq :+ c)
            }
          }
        }
      }

    }

    boat((wolf, sheep), (0, 0), Seq.empty)

    val re =
      result.map(_.length)

    if(re.isEmpty){
      0

    }else{
      re.min
    }
  }

}