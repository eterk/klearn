package org.eterk.klearn.LinkedList

import org.eterk.klearn.LeetCode

import scala.annotation.tailrec

object LinkedListCycle141 extends LeetCode[ListNode, Boolean] {
  override def no: Int = 141

  override def desc: String = "给定一个链表，判断链表中是否有环。"

  override def domain(input: ListNode): Boolean = input != null

  override def stdIO: Seq[(ListNode, Boolean)] = {
    // 示例输入输出对
    val node1 = new ListNode(3)
    val node2 = new ListNode(2)
    val node3 = new ListNode(0)
    val node4 = new ListNode(-4)
    node1.next = node2
    node2.next = node3
    node3.next = node4
    node4.next = node2 // 构成环

    val node5 = new ListNode(1)
    val node6 = new ListNode(2)
    node5.next = node6
    node6.next = node5 // 构成环

    val node7 = new ListNode(1) // 无环

    Seq(
      (node1, true),
      (node5, true),
      (node7, false),
      (null, false)
    )
  }

  override def set: Map[String, ListNode => Boolean] = {
    Map(
      "默认方法" -> hasCycle
    )
  }

  def hasCycle(head: ListNode): Boolean = {
    val map = collection.mutable.Map.empty[ListNode, ListNode]
    var next = head
    var res = false
    while (next != null && !res) {

      if (map.contains(next)) {
        res = true
      }
      map.addOne(next, next)
      next = next.next

    }
    res
  }
}

object ClimbStairs70 extends LeetCode[Int, Int] {
  override def no: Int = 70

  override def desc: String = "给定非负整数数组，你最初位于数组的第一个位置，数组中的每个元素代表你在该位置可以跳跃的最大长度。你的目标是使用最少的跳跃次数到达数组的最后一个位置。"

  override def domain(input: Int): Boolean = input >= 0 && input <= 2000

  override def stdIO: Seq[(Int, Int)] = {
    Seq(
      (2, 2),
      (5, 8),
      (10, 89)
    )
  }

  override def set: Map[String, Int => Int] = {
    Map(
      "默认方法" -> climbStairs
    )
  }

  def climbStairs(n: Int): Int = {
    var res = 1
    var res2 = 1
    for (i <- 2 to n) {
      var tmp = res2
      res2 = res
      res = res2 + tmp
    }
    res
  }

  def climbStairsRec(n: Int): Int = {

    var res = 1
    var res2 = 1
    var tmp = res

    @tailrec
    def climb(current: Int): Int = {
      if (current == n) return res2 + res
      tmp = res2
      res2 = res
      res = tmp + res2
      climb(current + 1)
    }

    n match {
      case 1 => res
      case n => climb(2)
    }

  }
}

object HouseRobber198 extends LeetCode[Array[Int], Int] {
  override def no: Int = 198

  override def desc: String = "你是一个职业强盗，计划沿街打劫房屋。每间房子都固定金钱数量，但是相邻的房子有防盗系统，如果两间相邻的房子在同一晚上被强盗闯入，系统会自动报警。计算在不触动警报装置的情况下，一夜之内能够偷窃到的最高金额。"

  override def domain(input: Array[Int]): Boolean = input.length >= 1 && input.length <= 100

  override def stdIO: Seq[(Array[Int], Int)] = {
    Seq(
      (Array(2, 7, 9, 3, 1), 12),
      (Array(3, 2, 3), 6),
      (Array(9, 2, 2, 8, 2), 17),
      (Array(1, 2, 3, 1), 4)
    )
  }

  override def set: Map[String, Array[Int] => Int] = {
    Map(
      "默认方法" -> houseRobber,
      "opt" -> rob
    )
  }

  def houseRobber(nums: Array[Int]): Int = {
    nums.length match {
      case 1 => nums.head
      case n =>
        var withoutLast = 0
        for (i <- (1 until nums.length)) {

          if (nums(i) + withoutLast > nums(i - 1)) {
            nums(i) = nums(i) + withoutLast
          } else {
            nums(i) = nums(i - 1)
          }
          withoutLast = nums(i - 1)
        }


    }

    nums.last
  }

  def rob(nums: Array[Int]): Int = {
    if (nums.length == 0) return 0
    if (nums.length == 1) return nums(0)
    if (nums.length == 2) return Math.max(nums(0), nums(1))

    val dp = new Array[Int](nums.length)
    dp(0) = nums(0)
    dp(1) = Math.max(nums(0), nums(1))

    for (i <- 2 until nums.length) {
      dp(i) = Math.max(dp(i - 1), dp(i - 2) + nums(i))
    }

    dp(nums.length - 1)
  }

}

object WordBreak139 extends LeetCode[(String, List[String]), Boolean] {
  override def no: Int = 139

  override def desc: String = "给定一个非空字符串 s 和一个包含非空单词列表的字典 wordDict，判定 s 是否可以被空格拆分为一个或多个在字典中出现的单词。"

  override def domain(input: (String, List[String])): Boolean = input._1.nonEmpty

  override def stdIO: Seq[((String, List[String]), Boolean)] = {
    Seq(
      (("leetcode", List("leet", "code")), true),
      (("applepenapple", List("apple", "pen")), true),
      (("a", List("b")), false),
      (("aaaaaaa", List("aaaa", "aaa")), true),
      (("catsandog", List("cats", "dog", "sand", "and", "cat")), false),
      (("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",
        List("a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa", "aaaaaaa", "aaaaaaaa", "aaaaaaaaa", "aaaaaaaaaa")), false)
    )
  }

  override def set: Map[String, ((String, List[String])) => Boolean] = {
    Map(
      "默认方法" -> wordBreak
    )
  }

  // todo 官方抄的答案都没通过,不知道为什么
  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    val wordSet = wordDict.toSet
    val dp = new Array[Boolean](s.length + 1)
    dp(0) = true

    import scala.util.control.Breaks.{breakable, break}
    breakable {
      for (i <- 1 to s.length) {
        for (j <- 0 until i) {
          if (dp(j) && wordSet.contains(s.substring(j, i))) {
            dp(i) = true
            break
          }
        }
      }

    }
    dp(s.length)
  }

  // 逻辑有问题
  def wordBreakWrong(s: String, wordDict: List[String]): Boolean = {
    val dp = collection.mutable.Buffer.empty[(String, Int, Int)]
    val map = wordDict.zip(wordDict).toMap
    var start = 0
    var i = 0

    while (i <= s.length) {

      val word = s.substring(start, i)
      println(start + "  " + i + "  :" + word)
      if (map.isDefinedAt(word)) {
        println(dp)
        dp.append((word, start, i))
        start = dp
          .filter(e => e._2 == dp.maxBy(_._2)._2)
          .minBy(_._3)._3
        println(start)
        i = start
      } else {
        i += 1
      }

    }


    dp.nonEmpty && dp.maxBy(_._3)._3 == s.length
  }


  /// 暴力求解效率低,超过运行时间
  def wordBreakRoubust(s: String, wordDict: List[String]): Boolean = {


    val map = wordDict.zip(wordDict).toMap

    val step = wordDict.maxBy(_.length).length

    def search(str: String): Boolean = {

      if (map.isDefinedAt(str)) return true
      val tmp = new StringBuilder()
      val dp = collection.mutable.Buffer.empty[String]

      for (i <- str.slice(0, step)) {
        tmp.append(i)
        if (map.isDefinedAt(tmp.result())) {
          dp.append(tmp.result())
        }
      }

      if (dp.isEmpty) {
        false
      } else {
        dp.exists(newStr => {
          search(str.slice(newStr.length, str.length))
        })
      }
    }

    search(s)
  }
}

object CoinChange322 extends LeetCode[(Array[Int], Int), Int] {
  override def no: Int = 322

  override def desc: String = "给定一个整数数组 coins 和一个整数 amount，计算并返回可以凑成总金额所需的最少的硬币个数。"

  override def domain(input: (Array[Int], Int)): Boolean = input._1.length > 0 && input._1.length <= 12

  override def stdIO: Seq[((Array[Int], Int), Int)] = {
    Seq(
      (Array(1, 2, 5), 11) -> 3,
      (Array(2), 3) -> -1,
      (Array(1), 0) -> 0,
      (Array(186, 419, 83, 408), 6249) -> 20
    )
  }

  override def set: Map[String, ((Array[Int], Int)) => Int] = {
    Map(
      "默认方法" -> coinChange
    )
  }

  //从大到小求解法失效,当硬币面额不定时
  def coinChange(coins: Array[Int], amount: Int): Int = {

    val coin = coins.sorted.reverse
    var res = 0
    var left = amount
    import scala.util.control.Breaks.{breakable, break}
    breakable {
      for (i <- coin.indices) {
        val num: Int = left / coin(i)
        left %= coin(i)
        res += num

        if (left == 0) break()

      }
    }
    println(amount + "  :" + res)
    if (left != 0) -1 else res

  }
}