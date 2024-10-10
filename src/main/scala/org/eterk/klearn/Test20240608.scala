package org.eterk.klearn

import org.eter.klearn.stack.BasicCalculator224
import org.eterk.klearn.LinkedList.{BestTimeToBuyAndSellStock121, ClimbStairs70, CoinChange322, HouseRobber198, LinkedListCycle141, MajorityElement169, MergeSortedArray88, RemoveDuplicates26, RemoveDuplicatesII80, RemoveElement27, WordBreak139}
import org.eterk.klearn.data_structure.{GameOfLife239, SpiralMatrix54}
import org.eterk.klearn.easy.{InsertInterval57, LeetCode1094, LeetCode252, LeetCode253, LeetCode435, LeetCode646, LongestCommonPrefix14, MergeIntervals56, MinimumNumberOfArrowsToBurstBalloons452, StrStr28, WordPattern290}
import org.eterk.klearn.untyped._

import java.util.Scanner
import scala.collection.mutable

object Test20240608 {
  def main(args: Array[String]): Unit = {

    //    CoinChange322.stdTest()

    //    println(fib1(4))
    //
    //    println(minCostClimbingStairs(Array(10, 15, 20)))
    //    println(minCostClimbingStairs(Array(1, 100, 1, 1, 1, 100, 1, 1, 100, 1)))
    //
    //    println(uniquePaths(3, 2))
    //    println(uniquePathsWithObstacles(Array(Array(0, 0, 0), Array(0, 1, 0), Array(0, 0, 0))))
    //    println(uniquePathsWithObstacles(Array(Array(0, 1), Array(0, 0))))

    //    pocker("299934KA10QA56".toSeq)
    //    pocker("3344556677889".toSeq)

    //    println(quack("quacqkuquacqkacuqkackuack"))
    //    println(quack("qququaauqccauqkkcauqqkcauuqkcaaukccakkck"))
    //    println(quack("quacqkuackquack"))
    //    println(quack("qaauucqcaa"))
    //    println(quack("quackquack"))
    //    println(jump(Array(2, 3, 1, 1, 4)))
    //    println(jump(Array(2, 3, 0, 1, 4)))
    //    println(jump(Array(0)))
    //    println(jump(Array(2, 1)))
    //    println(jump(Array(2, 3, 1)))

    //    println(combine(3, 2))
    //    println(combine1tok(3, 3))
    //    println(combinationSum3(3, 7))
    //    println(letterCombinations("23"))
    //    println(letterCombinations(""))
    //    println(letterCombinations("2"))
    //    println(letterCombinationsFlatMap("23"))
    //    println(letterCombinationsFlatMap(""))
    //    println(letterCombinationsFlatMap("2"))



    println(splitStr("aac"))
    println(printer(Array(4, 45, 42, 12)))

    //    CombinationSum39.stdTest()
    //    CombinationSum40.stdTest()

    //    println(longestStr("AAAAHHHBBCDHHHH", 3) == 1)
    //    println(longestStr("AABAAA", 2) == 1)
    //    println(longestStr("ABC", 4) == -1)

    //    println(search(Array(-1, 0, 3, 5, 9, 12), 13))


    val grid: Seq[Seq[String]] = Seq(
      Seq('A', 'C', 'C', 'F'),
      Seq('C', 'D', 'E', 'D'),
      Seq('B', 'E', 'S', 'S'),
      Seq('F', 'E', 'C', 'A')
    ).map(_.map(_.toString))

    val grid1: Seq[Seq[String]] = Seq(
      Seq('A', 'C', 'C'),
      Seq('C', 'D', 'E'),
      Seq('B', 'E', 'S'),
      Seq('F', 'E', 'C')
    ).map(_.map(_.toString))


    println(searchWord(grid, "ACCESS"))
    println(searchWord(grid1, "ACBF"))
  }

  def searchWord(matrix: Seq[Seq[String]], word: String): Option[Seq[(Int, Int)]] = {
    val go = (index: (Int, Int), move: (Int, Int)) => {
      val newX = index._1 + move._1
      val newY = index._2 + move._2
      val illegal1 = newX < 0 || newX >= matrix.length
      val illegal2 = newY < 0 || newY >= matrix.head.length

      if (illegal1 || illegal2) {
        None
      } else {
        Some((newX, newY))
      }

    }

    val find: Seq[((Int, Int)) => Option[(Int, Int)]] = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
      .map(i => go(_, i))

    val mValue = (index: (Int, Int)) => matrix(index._1)(index._2)

    var continue = true

    var result1: Seq[(Int, Int)] = null

    def search(result: Seq[(Int, Int)], currentTarget: Int): Seq[(Int, Int)] = {
      if (currentTarget == word.length) {
        continue = false
        result1 = result.dropRight(1)
        return result
      }
      if (mValue(result.last) == word(currentTarget).toString) {
        val res =
          find.map(f => f(result.last))
            .collect {
              case Some(i) if !result.contains(i) =>
                search(result :+ i, currentTarget + 1)
            }.filterNot(_.isEmpty)

        if (res.isEmpty) {
          Seq.empty
        } else {
          res.head
        }
      } else {
        Seq.empty
      }
    }

    for (i <- matrix.indices) {
      for (j <- matrix.head.indices) {
        if (continue) {
          search(Seq(i -> j), 0)
        }
      }
    }
    Option(result1)
  }

  


  def boss(teamNum: Int, data: Seq[(Int, Int, Int)]): (Int, Int) = {
    val parent = data.map(_._2).distinct
    val child = data.map(_._1)
    val boss = parent.find(x => child.exists(c => x != c)).get


    ???
  }

  def printer(i: Array[Int]) = {

    import scala.collection.mutable

    // 创建一个双向队列，存储任务及其初始位置
    val queue = mutable.Queue.empty[(Int, Int)]

    for (index <- i.indices) {
      queue.enqueue((i(index), index))
    }

    val result = mutable.Buffer.empty[Int]

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (queue.exists(_._1 > current._1)) {
        // 如果队列中有优先级更高的任务，将当前任务移到队列尾部
        queue.enqueue(current)
      } else {
        // 否则执行当前任务
        result.append(current._2)
      }
    }

    result.toList
  }

  // 示例输入


  // 131  可以使用动态规划实现
  def splitStr(s: String): List[List[String]] = {
    val result = collection.mutable.Buffer.empty[List[String]]

    val isHuiWen = (str: String) => str == str.reverse

    def back(startIndex: Int, v: Vector[String]): Unit = {

      if (startIndex == s.length) {
        if (isHuiWen(v.head)) {
          result.append(v.toList.reverse)
        }
        return
      }
      val current = s(startIndex).toString

      back(startIndex + 1, (v.head ++ current) +: v.tail)

      if (isHuiWen(v.head)) {
        back(startIndex + 1, current +: v)
      }
    }

    back(1, Vector(s.head.toString))

    result.toList

  }

  def check(result: String, guess: String): String = {
    val map: Map[Boolean, Seq[(Char, Char)]] = result.zip(guess).groupBy(x => x._1 == x._2).withDefaultValue(Seq.empty)
    val b =
      if (map(false).nonEmpty) {
        val (r, g) = map(false).unzip
        g.count(c => r.contains(c)) + "B"
      } else {
        "0B"
      }
    val a = map(true) + "A"
    a + b
  }

  def guess(result: String, input: String): Boolean = {
    val str = check(result, input)
    if (str == "4A0B") {
      true
    } else {
      println(str)
      false
    }
  }


  /**
   * leetcode 77
   *
   * @param n
   * @param k
   * @return
   */
  def combine(n: Int, k: Int): List[List[Int]] = {
    val path = collection.mutable.Buffer.empty[Int]
    val result = collection.mutable.Buffer.empty[List[Int]]

    def backtrack(startIndex: Int): Unit = {
      // 减枝
      if (path.length + n - startIndex + 1 < k) {
        return
      }

      if (path.length == k) {
        result.append(path.toList)
      }
      for (elem <- (startIndex to (n))) {
        path.append(elem)
        backtrack(elem + 1)
        path.remove(path.length - 1)
      }
    }

    backtrack(1)

    result.toList
  }

  /**
   * leetcode  216
   */
  def combinationSum3(k: Int, n: Int): List[List[Int]] = {
    val result = collection.mutable.Buffer.empty[List[Int]]
    val path = collection.mutable.Buffer.empty[Int]

    def backTrack(start: Int): Unit = {
      if (path.length == k && path.sum == n) {
        result.append(path.toList)
        return
      }
      if (path.length >= k || path.sum >= n) return

      for (elem <- start to 9) {
        path.append(elem)
        backTrack(elem + 1) // 更新为 elem + 1
        path.remove(path.length - 1)
      }
    }

    backTrack(1)
    result.toList
  }

  /**
   *
   * 最长连续字串
   */
  def longestStr(str: String, num: Int) = {

    val map = collection.mutable.Map.empty[Char, Int]
    val iter = (str.tail ++ " ").iterator
    var last = str.head
    var size = 1

    while (iter.hasNext) {
      val current = iter.next()
      if (current == last) {
        size += 1
      } else {
        if (map.isDefinedAt(last)) {
          if (map(last) < size) {
            map.update(last, size)
          }
        } else {
          map.addOne(last -> size)
        }
        last = current
        size = 1
      }
    }

    val res: Seq[(Int, mutable.Map[Char, Int])] = map.groupBy(_._2).toSeq.sortBy(_._1).reverse
    //      .sortBy(_._1).reverse

    println(res)

    if (res.length < num) {
      -1
    } else {

      res(num - 1)._1
    }


  }

  /**
   * leetcode 29  回溯算法
   *
   * @param digits 2 -9 所有字母的组合
   * @return
   */
  def letterCombinationsFlatMap(digits: String): List[String] = {
    if (digits.isEmpty) return List.empty
    val dict = Seq(Seq(), Seq(),
      Seq('a', 'b', 'c'),
      Seq('d', 'e', 'f'),
      Seq('g', 'h', 'i'),
      Seq('j', 'k', 'l'),
      Seq('m', 'n', 'o'),
      Seq('p', 'q', 'r', 's'),
      Seq('t', 'u', 'v'),
      Seq('w', 'x', 'y', 'z'))
      .map(_.map(_.toString()))

    digits
      .map(c => dict(c.toString.toInt))
      .reduceLeft((seq1, seq2) =>
        seq1.flatMap(c1 => seq2.flatMap(c2 => List(c1 + c2)))
      ).toList


  }

  /**
   * leetcode 29  回溯算法
   *
   * @param digits 2 -9 所有字母的组合
   * @return
   */
  def letterCombinations(digits: String): List[String] = {
    if (digits.isEmpty) return List.empty
    val dict = Seq(Seq(), Seq(),
      Seq('a', 'b', 'c'),
      Seq('d', 'e', 'f'),
      Seq('g', 'h', 'i'),
      Seq('j', 'k', 'l'),
      Seq('m', 'n', 'o'),
      Seq('p', 'q', 'r', 's'),
      Seq('t', 'u', 'v'),
      Seq('w', 'x', 'y', 'z'))
    val result = collection.mutable.Buffer.empty[String]
    val path = new StringBuffer()


    def backtrack(startIndex: Int): Unit = {
      if (path.length == digits.length) {
        result.append(path.toString)
        return
      }

      val index = digits(startIndex).toString.toInt

      for (elem <- dict(index)) {
        path.append(elem)
        backtrack(startIndex + 1)
        path.deleteCharAt(path.length - 1)

      }

    }

    backtrack(0)

    result.toList
  }


  /**
   * 求出n 个数中 小于 k 个数的所有组合
   *
   * @param n
   * @param k
   * @return
   */
  def combine1tok(n: Int, k: Int): List[List[Int]] = {
    val path = collection.mutable.Buffer.empty[Int]
    val result = collection.mutable.Buffer.empty[List[Int]]

    def backtrack(startIndex: Int, k: Int): Unit = {
      if (path.length == k) {
        result.append(path.toList)
      }
      for (elem <- (startIndex until (n))) {
        path.append(elem)
        backtrack(elem + 1, k)
        path.remove(path.length - 1)
      }
    }

    (1 to k).foreach(k => backtrack(0, k))


    result.toList
  }


  /**
   * leetcode  75
   *
   * @param nums
   * @return
   */
  def jump(nums: Array[Int]): Int = {
    val line = nums.zipWithIndex.map {
      case (step, i) => i -> (i + step)
    }
    var end = 0
    var step = 0

    while (end < nums.length - 1) {
      end = line
        .filter(i => i._1 <= end)
        .maxBy(_._2)._2

      step += 1
    }

    step
  }

  def guessAndCheck(input: Seq[(String, String)]): String = {
    var all = (0 to 9999).map(i => "%04d".format(i))

    input.foreach {
      case (v, r) =>
        all = all.filter(res => check(res, v) == r)
    }

    if (all.length == 1) {
      all.head
    } else {
      "NA"
    }


  }


  def fib(n: Int): Int = {
    val dp = collection.mutable.Buffer[Int](0, 1)
    var i = 2
    while (i <= n) {
      dp.append(dp(i - 1) + dp(i - 2))
      i += 1
    }
    dp(n)
  }

  def fib1(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case other =>
        var i = 0
        var j = 1
        var k = 2
        var res = 0
        while (k <= n) {
          res = i + j
          i = j
          j = res
          k += 1
        }

        res
    }

  }

  def minCostClimbingStairs(cost: Array[Int]): Int = {

    var i = 0
    var j = 0
    var kI = 2

    val getK = () => Math.min(cost(kI - 1) + j, cost(kI - 2) + i)


    while (kI < cost.length) {
      var k = getK()
      i = j
      j = k
      kI += 1
    }

    getK()
  }

  def uniquePaths(m: Int, n: Int): Int = {
    val dp =
      (0 until m).map(i => {
        if (i == 0) {
          Array.fill(n)(1)
        } else {
          1 +: Array.fill(n - 1)(0)
        }
      })

    for (i <- 1 until m) {
      for (j <- 1 until n) {
        dp(i)(j) = dp(i - 1)(j) + dp(i)(j - 1)
      }
    }
    dp(m - 1)(n - 1)
  }

  def search(nums: Array[Int], target: Int): Int = {

    def rec(arr: Array[(Int, Int)]): Int = {
      if (arr.length == 1 && arr.head._1 != target || arr.isEmpty) {
        return -1
      }
      val mid = arr.length / 2


      arr(mid) match {
        case (i, j) if i < target =>
          rec(arr.slice(mid + 1, nums.length))
        case (i, j) if i == target =>
          j
        case (i, j) if i > target =>
          rec(arr.slice(0, mid))

      }
    }

    rec(nums.zipWithIndex)
  }

  def quack(context: String): Int = {
    val voice = "quack"
    val birds = collection.mutable.Buffer[Char]()

    val next = (c: Char) => {
      c match {
        case 'k' => 'q'
        case o => voice(voice.indexOf(o) + 1)
      }
    }

    context.foreach(c => {
      if (birds.isEmpty && c == 'q') {
        birds.append(next(c))
      } else {
        val index: Int = birds.indexWhere(i => i == c, 0)
        if (index == -1 && c == 'q') {
          birds.append(next(c))
        } else if (index != -1) {
          birds(index) = next(c)
        }


      }
    })

    println(birds)

    val res =
      birds.filter(_ == 'q')

    if (res.isEmpty) {
      -1
    } else {
      res.length
    }

  }

  def pocker(seq: Seq[Char]): String = {
    val value: PartialFunction[Char, Int] = {
      case 'J' => 8
      case 'Q' => 9
      case 'K' => 10
      case '1' => 12
      case '2' => 13
      case c => c.toInt - 2
    }
    val seqChar = seq
      .groupBy(identity)
      .toArray
      .flatMap(c => c._2.zipWithIndex.map(c => c._2 -> c._1))
      .groupBy(_._1)
      .toSeq
      .map(c => c._1 -> c._2.map(_._2))
      .map(c => c._1 -> c._2.filter(c => value(c) != 13).sortBy(value))
      .foreach { case (i, array) => {
        if (array.length >= 5) {
          var last: Int = -1
          var i = 0
          var len = 0
          while (i < array.length) {
            if (last != -1) {
              val current = value(array(i))
              if (current == last + 1) {
                len += 1

              } else {
                if (len >= 5) {
                  println(array.slice(i - len, i).toSeq)
                }
                len = 1
              }
              last = current
            } else {
              len = 1
              last = array(i)
            }

            i += 1
          }
          if (len >= 5) {
            println(array.slice(i - len, i).toSeq)
          }

        }
      }
      }
    ""

  }


  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    if (obstacleGrid.head.head == 1 || obstacleGrid.last.last == 1) return 0
    val rowHeadIndex = {
      val i = obstacleGrid.head.indexOf(1)
      if (i == -1) {
        Int.MaxValue
      } else {
        i
      }

    }

    val colHeadIndex = obstacleGrid
      .zipWithIndex
      .find(arr => if (arr._1(0) == 1) true else false)
      .map(_._2).getOrElse(Int.MaxValue)


    val dp =
      obstacleGrid.indices
        .map(i => {
          obstacleGrid.head.indices
            .map(j => {
              if (obstacleGrid(i)(j) == 1) {
                0
              } else if ((i == 0 && j >= rowHeadIndex) || (i >= colHeadIndex && j == 0)) {
                0
              } else {
                1
              }
            }).toArray
        })


    dp.indices.tail.foreach(i => dp.head.indices.tail.foreach(j => {
      if (dp(i)(j) != 0) {
        dp(i)(j) = dp(i - 1)(j) + dp(i)(j - 1)
      }


    }))
    dp.last.last
  }


  def doTwoDay() = {
    ClimbStairs70.stdTest()
    HouseRobber198.stdTest()
    WordBreak139.stdTest() // todo 失败
  }

  def doOneDay(): Unit = {
    //    println(BasicCalculator224.mp("2-1+2"))
    println("-1".toInt)
    SameTree100.stdTest()
    MaximumDepthOfBinaryTree104.stdTest()
    InvertBinaryTree226.stdTest()
    SymmetricTree101.stdTest()
    BuildTree105.stdTest()
    CombinationSum39.stdTest()
    CombinationSum40.stdTest()
    BuildTree106.stdTest()
    LinkedListCycle141.stdTest()
  }


  def do20240613() = {
    BasicCalculator224.stdTest()
    LeetCode646.stdTest()
    LeetCode435.stdTest()
    MergeSortedArray88.stdTest()
    RemoveElement27.stdTest()
    RemoveDuplicates26.stdTest()
    RemoveDuplicatesII80.stdTest()
    MajorityElement169.stdTest()
    BestTimeToBuyAndSellStock121.stdTest()
  }

  def do20240612() = {
    MergeIntervals56.stdTest()
    MinimumNumberOfArrowsToBurstBalloons452.stdTest()
    InsertInterval57.stdTest()
    LeetCode252.stdTest()
    LeetCode253.stdTest()
    LeetCode435.stdTest()
    LeetCode1094.stdTest()
  }

  def do20240611() = {
    StrStr28.stdTest()
    LongestCommonPrefix14.stdTest()
    WordPattern290.stdTest()
  }

  def do20240608() = {
    HIndex274.stdTest()

    ReverseWordsInString.stdTest()

    IntegerToRoman.stdTest()

  }

  def do20240609() = {
    ValidPalindrome125.stdTest()

    Subsequence392.stdTest()

    ThreeSum15.stdTest()

    TwoSum167.stdTest()


    ValidSudoku36.stdTest()

    SetMatrixZeroes73.stdTest()


    CanConstruct383.stdTest()


    IsomorphicString205.stdTest()

  }

  def do20240610(): Unit = {
    LongestConsecutiveSeq128
      .stdTest()

    TwoSum1
      .stdTest()


    GroupAnagrams49
      .stdTest()

    GameOfLife239
      .stdTest()


    SpiralMatrix54
      .stdTest()

  }


}
