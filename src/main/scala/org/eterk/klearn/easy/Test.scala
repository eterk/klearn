package org.eterk.klearn.easy

import org.eterk.klearn.LeetCode


object StrStr28 extends LeetCode[(String, String), Int] {
  override def no: Int = 28

  override def desc: String = "实现 strStr() 函数。给定一个 haystack 字符串和一个 needle 字符串，在 haystack 字符串中找出 needle 字符串出现的第一个位置 (从0开始)。如果不存在，则返回 -1。"

  override def domain(input: (String, String)): Boolean = true

  override def stdIO: Seq[((String, String), Int)] = Seq((("hello", "ll"), 2), (("a", "a"), 0), (("aaaaa", "bba"), -1))

  override def set: Map[String, ((String, String)) => Int] = Map("" -> strStr)

  def strStr(haystack: String, needle: String): Int = {
    import scala.util.control.Breaks._
    var res = -1
    breakable {
      for (i <- haystack.indices) {
        val last = i + needle.length
        if (haystack.length >= last) {
          if (haystack.slice(i, last) == needle) {
            res = i
            break
          }
        }
      }

    }
    res
  }
}

// LeetCode 14. 最长公共前缀
object LongestCommonPrefix14 extends LeetCode[Array[String], String] {
  override def no: Int = 14

  override def desc: String = "编写一个函数来查找字符串数组中的最长公共前缀。如果不存在公共前缀，返回空字符串 \"\"。"

  override def domain(input: Array[String]): Boolean = true

  override def stdIO: Seq[(Array[String], String)] =
    Seq(
      (Array(), ""),
      (Array("flow"), "flow"),
      (Array("flower", "flow", "flight"), "fl"),
      (Array("dog", "racecar", "car"), "")
    )

  override def set: Map[String, Array[String] => String] = Map("easy" -> longestCommonPrefix)

  def longestCommonPrefix(strs: Array[String]): String = {
    if (strs.isEmpty) return ""
    val head = strs.head
    var i = 0
    import scala.util.control.Breaks._
    breakable {
      while (i < head.length) {
        for (j <- strs.tail) {
          if (j.length < i + 1 || head(i) != j(i)) break
        }
        i += 1
      }
    }
    strs.head.slice(0, i)
  }
}

// LeetCode 290. 单词规律
object WordPattern290 extends LeetCode[(String, String), Boolean] {
  override def no: Int = 290

  override def desc: String = "给定一种规律 pattern 和一个字符串 str ，判断 str 是否遵循相同的规律。这里的 遵循 指完全匹配，例如， pattern 里的每个字母和字符串 str 中的每个非空单词之间存在着双向连接的对应规律。"

  override def domain(input: (String, String)): Boolean = true

  override def stdIO: Seq[((String, String), Boolean)] = Seq(
    (("abba", "d c c f"), false),
    (("abba", "dog cat cat fish"), false),
    (("abba", "d c c d"), true),
    (("abba", "d d d d"), false),
    (("aaa", "aa aa aa aa"), false),
    (("jquery", "jquery"), false),
    (("abba", "dog cat cat dog"), true)

  )

  override def set: Map[String, ((String, String)) => Boolean] = Map("哈希表" -> wordPattern, "h2" -> wordPattern1)

  //随便写的 有问题
  def wordPattern1(pattern: String, s: String): Boolean = {
    val ele = s.split(' ')
    if (ele.length != pattern.length) return false
    val cs = pattern.zip(ele).toMap
    val sc = cs.map(_.swap).toMap
    if (cs.size != sc.size) return false
    val c1 =
      cs.forall {
        case (c, s) => sc(s) == c
      }
    val c2 =
      sc.forall {
        case (s, c) => cs(c) == s
      }
    c1 && c2


  }

  /**
   * 这道题没有使用内置split 函数处理,导致做的很慢
   */
  def wordPattern(pattern: String, s: String): Boolean = {
    val mapCS = collection.mutable.Map[Char, String]()
    val mapSc = collection.mutable.Map[String, Char]()

    def put(c: Char, s: String): Unit = {
      mapCS.addOne(c -> s)
      mapSc.addOne(s -> c)
    }

    def legal(c: Char, s: String): Boolean = {
      val sS = mapCS.get(c)
      val cS = mapSc.get(s)
      sS.forall(ss => ss == s) && cS.forall(cs => cs == c)
    }

    val tmpBuffer = new StringBuilder()
    var charI = 0
    var strSize = 0
    import scala.util.control.Breaks._
    breakable {
      for (i <- s.indices) {
        val char = s(i)
        val isLast = i == s.length - 1
        if (isLast || char != ' ') {
          tmpBuffer.append(char)
        }
        if (!pattern.isDefinedAt(charI)) break()
        if (char == ' ' || isLast) {
          strSize += 1
          val str = tmpBuffer.result()
          val charK = pattern(charI)

          if (legal(charK, str)) {
            put(charK, str)
          } else {
            break()
          }
          charI += 1
          tmpBuffer.clear()
        }
      }
    }
    strSize == pattern.size && tmpBuffer.isEmpty
  }
}

object SummaryRanges228 {

  def summaryRanges(nums: Array[Int]): List[String] = {
    if (nums.isEmpty) return List.empty
    var lastStart = nums.head
    var lastEnd = nums.head
    val res = collection.mutable.ListBuffer[String]()

    def append() = {
      val ele =
        if (lastStart == lastEnd) {
          lastEnd.toString
        } else {
          s"$lastStart->$lastEnd"
        }
      res.append(ele)
    }

    for (i <- nums.tail) {
      if (i != lastEnd + 1) {
        append()
        lastStart = i
      }
      lastEnd = i
    }
    append()
    res.toList
  }
}

// LeetCode 56. 合并区间
object MergeIntervals56 extends LeetCode[Array[Array[Int]], Array[Array[Int]]] {
  override def no: Int = 56

  override def desc: String = "给出一个区间的集合，请合并所有重叠的区间。"

  override def domain(input: Array[Array[Int]]): Boolean = true

  override def stdIO: Seq[(Array[Array[Int]], Array[Array[Int]])] = Seq(
    ((Array(Array(1, 3), Array(2, 6), Array(8, 10), Array(15, 18))), Array(Array(1, 6), Array(8, 10), Array(15, 18))),
    ((Array(Array(1, 4), Array(4, 5))), Array(Array(1, 5)))
  )

  override def set: Map[String, Array[Array[Int]] => Array[Array[Int]]] = Map(
    "默认方法" -> merge
  )

  /**
   * 代码有一定问题，但是最后还是通过了leetcode 的测试
   */
  def s1(intervals: Array[Array[Int]]): Array[Array[Int]] = {

    val sorted = intervals.sortBy(i => i(1))

    //    printStr(sorted)

    def rec(head: Array[Int], tail: Array[Array[Int]]): Array[Array[Int]] = {
      if (tail.isEmpty) return Array(head)

      val Array(s1, e1) = head // ()
      val Array(s2, e2) = tail.head //[]

      if (s1 >= s2 && s2 <= e1) {
        rec(Array(s2, e2), tail.tail)
      } else if (s2 >= s1 && s2 <= e1) {
        rec(Array(s1, e2), tail.tail)
      } else {
        head +: rec(tail.head, tail.tail)
      }

    }

    val r = rec(sorted.head, sorted.tail).sortBy(_(1))
    val r1 = rec(r.head, r.tail).sortBy(_(1))
    val r2 = rec(r1.head, r1.tail).sortBy(_(1))
    rec(r2.head, r2.tail)

  }


  def s2(intervals: Array[Array[Int]]): Array[Array[Int]] = {

    val combine = (v1: Array[Int], v2: Array[Int]) => {
      if (v1(0) <= v2(0) && v1(1) < v2(1)) {
        Array(Array(v1(0), v2(1)))
      } else if (v2(0) <= v1(0) && v2(1) < v1(1)) {
        Array(Array(v2(0), v1(1)))
      } else {
        Array(v1, v2)
      }
    }
    intervals
  }

  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    if (intervals.length == 1) return intervals
    val sorted = intervals.sortBy(h => h.head)
    var res = collection.mutable.Buffer(sorted.head)
    for (current <- sorted.tail) {
      val Array(c1, c2) = current
      val Array(l1, l2) = res.last
      //因为排序的原因 一定存在 l1<=c1
      if (c1 <= l2) {
        // 可以合并的
        if (c2 > l2) {
          res.last(1) = c2
        }

      } else {
        res.append(current)
      }

    }
    res.toArray
  }
}

// LeetCode 57. 插入区间
object InsertInterval57 extends LeetCode[(Array[Array[Int]], Array[Int]), Array[Array[Int]]] {
  override def no: Int = 57

  override def desc: String = "给出一个无重叠的 ，按照区间起始端点排序的区间列表。在列表中插入一个新的区间，你需要确保列表中的区间仍然有序且不重叠（如果有必要的话，可以合并区间）。"

  override def domain(input: (Array[Array[Int]], Array[Int])): Boolean = true

  override def stdIO: Seq[((Array[Array[Int]], Array[Int]), Array[Array[Int]])] = Seq(
    (((Array(Array(1, 3), Array(6, 9)), Array(2, 5))), Array(Array(1, 5), Array(6, 9))),
    (((Array(Array(1, 2), Array(3, 5), Array(6, 7), Array(8, 10), Array(12, 16)), Array(4, 8))),
      Array(Array(1, 2), Array(3, 10), Array(12, 16)))
  )

  /**
   * o(n(log(n))
   */
  def insert2(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    val sorted = (intervals :+ newInterval).sortBy(_.head)
    MergeIntervals56.merge(sorted)
  }

  def insert1(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    val sortedIntervals = (intervals :+ newInterval).sortWith(_(0) < _(0))
    val res = collection.mutable.ArrayBuffer[Array[Int]]()

    for (interval <- sortedIntervals) {
      if (res.isEmpty || res.last(1) < interval(0)) {
        res.append(interval)
      } else {
        res.last(1) = math.max(res.last(1), interval(1))
      }
    }

    res.toArray
  }

  val compareIndex = (c: Array[Int], l: Array[Int]) => {
    if (c(1) < l(0)) {
      "前排"
    } else if (c(0) > l(1)) {
      "后排"
    } else if (c(0) <= l(0) && c(1) >= l(1)) {
      "包含"
    } else if (c(0) >= l(0) && c(1) <= l(1)) {
      "被包含"
    } else if (c(0) <= l(0) && c(1) < l(1)) {
      "露头"
    } else if (c(0) > l(0) && c(1) >= l(1)) {
      "露尾"
    } else {
      "error"
    }
  }

  /**
   * o(N) 自己的想法
   */
  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {

    if (intervals.isEmpty) return Array(newInterval)
    val res = collection.mutable.Buffer[Array[Int]](newInterval)

    val handle = (relation: String, current: Array[Int], last: Array[Int]) => {
      relation match {
        case "前排" =>
          res(res.length - 1) = current
          res.append(last)
        case "后排" =>
          res.append(current)
        case "包含" =>
          res(res.length - 1) = current
        case "被包含" =>
        case "露头" => res.last(0) = current(0)
        case "露尾" => res.last(1) = current(1)
        case "error" =>

      }

    }

    for (current <- intervals) {
      val last = res.last
      val relation = compareIndex(current, last)
      handle(relation, current, last)
      //      println(relation + " " + current.toSeq + " " + last.toSeq + "    res:" + res.map(_.toSeq))
    }

    res.toArray

  }

  /**
   * 实现类默认实现set 方法体为 Map("实例方法"->小驼峰命名的方法)
   */
  override def set: Map[String, ((Array[Array[Int]], Array[Int])) => Array[Array[Int]]] = Map(
    "插入-遍历-比较" -> insert,
    "插入-排序-合并" -> insert1
  )
}

// LeetCode 452. 用最少数量的箭引爆气球
object MinimumNumberOfArrowsToBurstBalloons452 extends LeetCode[Array[Array[Int]], Int] {
  override def no: Int = 452

  override def desc: String = "在二维空间中有许多球形的气球。对于每个气球，提供的输入是水平方向上，气球直径的开始和结束坐标。由于它是水平的，所以气球的高度并不重要。起始坐标总是小于结束坐标。平面内最多存在104个气球。一支弓箭可以沿着 x 轴从不同点完全垂直地射出。在坐标 x 处射出一支箭，若有一个气球的直径的开始和结束坐标为 xstart，xend， 且满足  xstart ≤ x ≤ xend，则该气球会被引爆。可以射出的弓箭的数量没有限制。 弓箭一旦被射出之后，可以无限地前进。我们想找到使得所有气球全部被引爆，所需的弓箭的最小数量。"

  override def domain(input: Array[Array[Int]]): Boolean = true

  override def stdIO: Seq[(Array[Array[Int]], Int)] = Seq(
    //    ((Array(Array(10, 16), Array(2, 8), Array(1, 6), Array(7, 12))), 2),
    //    ((Array(Array(1, 2), Array(3, 4), Array(5, 6), Array(7, 8))), 4),
    //    ((Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(4, 5))), 2),
    ((Array(Array(-1, 1), Array(0, 1), Array(2, 3), Array(1, 2))), 2)
    //    (Array(Array(1, 1)), 1)
  )

  override def set: Map[String, Array[Array[Int]] => Int] = Map(
    "默认方法" -> findMinArrowShots
  )

  def findMinArrowShots(points: Array[Array[Int]]): Int = {
    if (points.length == 1) return 1
    val sorted = points.sortBy(_.head)
    val res = collection.mutable.Buffer[Array[Int]](sorted.head)

    val compareIndex = (c: Array[Int], l: Array[Int]) => {
      if (c(1) < l(0)) {
        "前排"
      } else if (c(0) > l(1)) {
        "后排"
      } else if (c(0) <= l(0) && c(1) >= l(1)) {
        "包含"
      } else if (c(0) >= l(0) && c(1) <= l(1)) {
        "被包含"
      } else if (c(0) <= l(0) && c(1) < l(1)) {
        "露头"
      } else if (c(0) > l(0) && c(1) >= l(1)) {
        "露尾"
      } else {
        "error"
      }
    }

    val handle = (relation: String, c: Array[Int], l: Array[Int]) => {
      relation match {
        case "前排" =>
          res(res.length - 1) = c
          res.append(l)
        case "后排" =>
          res.append(c)
        case "包含" =>
        case "被包含" =>
          res(res.length - 1) = c
        case "露头" =>
          res(res.length - 1) = Array(l(0), c(1))
        case "露尾" =>
          res(res.length - 1) = Array(c(0), l(1))
      }
    }

    for (current <- sorted.tail) {
      val last = res.last
      val relation = compareIndex(current, last)
      //      print(relation + " " + current.toSeq + " " + last.toSeq)
      handle(relation, current, last)
      //      println("    res:" + res.map(_.toSeq))
    }

    res.size

  }

  val compareIndex = (c: Array[Int], l: Array[Int]) => {
    if (c(1) < l(0)) {
      "前排"
    } else if (c(0) > l(1)) {
      "后排"
    } else if (c(0) <= l(0) && c(1) >= l(1)) {
      "包含"
    } else if (c(0) >= l(0) && c(1) <= l(1)) {
      "被包含"
    } else if (c(0) <= l(0) && c(1) < l(1)) {
      "露头"
    } else if (c(0) > l(0) && c(1) >= l(1)) {
      "露尾"
    } else {
      "error"
    }
  }

}

object LeetCode252 extends LeetCode[Array[Array[Int]], Boolean] {
  override def no: Int = 252

  override def desc: String = "给定一个会议时间安排的数组 intervals ，每个会议时间都会包括开始和结束的时间 intervals[i] = [starti, endi] ，请你判断一个人是否能够参加这里面的全部会议。"

  override def domain(input: Array[Array[Int]]): Boolean = input.length >= 0 && (input.length <= (10 ^ 4))

  override def stdIO: Seq[(Array[Array[Int]], Boolean)] = {
    Seq(
      (Array(Array(0, 30), Array(5, 10), Array(15, 20)), false),
      (Array(Array(13, 15), Array(1, 13)), true),
      (Array(Array(7, 10), Array(2, 4)), true)
    )
  }

  override def set: Map[String, Array[Array[Int]] => Boolean] = {
    Map(
      "默认方法" -> canAttendMeetings
    )
  }

  def canAttendMeetings(intervals: Array[Array[Int]]): Boolean = {
    if (intervals.length <= 1) return true
    var res = true
    val sorted = intervals.sortBy(_.head)
    var last: Array[Int] = sorted.head
    var lastEnd = last(1)

    import scala.util.control.Breaks._
    breakable {
      for (current <- sorted.tail) {
        if (lastEnd > current(0)) {
          res = false
          break()
        }
        lastEnd = Math.max(current(1), last(1))
        last = current
      }
    }

    res
  }
}

object LeetCode253 extends LeetCode[Array[Array[Int]], Int] {
  override def no: Int = 253

  override def desc: String = "给你一个会议时间安排的数组 intervals ，每个会议时间都会包括开始和结束的时间 intervals[i] = [starti, endi] ，返回 所需会议室的最小数量 。"

  override def domain(input: Array[Array[Int]]): Boolean = input.length >= 1 && input.length <= (10 ^ 4)

  override def stdIO: Seq[(Array[Array[Int]], Int)] = {
    Seq(
      (Array(Array(0, 30), Array(5, 10), Array(15, 20)), 2),
      (Array(Array(2, 15), Array(4, 9), Array(9, 26), Array(16, 23), Array(36, 45)), 2),
      (Array(Array(1, 5), Array(8, 9), Array(8, 9)), 2),
      (Array(Array(7, 10), Array(2, 4)), 1)
    )
  }

  override def set: Map[String, Array[Array[Int]] => Int] = {
    Map(
      "默认方法" -> minMeetingRooms
    )
  }

  //todo 没做出来!!!!
  def minMeetingRooms(intervals: Array[Array[Int]]): Int = {
    if (intervals.length <= 1) return intervals.length
    val sorted = intervals.sortBy(_.head)
    var last = sorted.head
    var num = 1
    val other = collection.mutable.Buffer.empty[Array[Int]]

    for (current <- sorted.tail) {
      if (current(0) < last(1)) {
        //        num += 1
        if (current(1) < last(1)) {
          other.append(last)
          last = current
        } else {
          other.append(current)
        }
      } else {
        if (current(1) > last(1)) {
          last = current
        }
      }

    }

    val o1 = other.toSeq.map(_.toSeq)
    val u1 = intervals.toSeq.map(_.toSeq).diff(o1)
    //    println(num + " " + u1 + "  " + o1)
    num + minMeetingRooms(other.toArray)

  }
}

object LeetCode1094 extends LeetCode[(Array[Array[Int]], Int), Boolean] {
  override def no: Int = 1094

  override def desc: String = "车上最初有 capacity 个空座位。车 只能 向一个方向行驶（也就是说，不允许掉头或改变方向）。给定整数 capacity 和一个数组 trips ,  trip[i] = [numPassengersi, fromi, toi] 表示第 i 次旅行有 numPassengersi 乘客，接他们和放他们的位置分别是 fromi 和 toi 。这些位置是从汽车的初始位置向东的公里数。当且仅当你可以在所有给定的行程中接送所有乘客时，返回 true，否则请返回 false。"

  override def domain(input: (Array[Array[Int]], Int)): Boolean = input._1.length >= 1 && input._1.length <= 1000

  override def stdIO: Seq[((Array[Array[Int]], Int), Boolean)] = {
    Seq(
      //      ((Array(Array(2, 1, 5), Array(3, 3, 7)), 4), false),
      //      ((Array(Array(2, 1, 5), Array(3, 3, 7)), 5), true),
      ((Array(Array(10, 5, 7), Array(10, 3, 4), Array(7, 1, 8), Array(4, 3, 4)), 24), true)
      //      ((Array(Array(3, 2, 8), Array(4, 4, 6), Array(10, 8, 9)), 11), true),
    )
  }


  def carPooling(trips: Array[Array[Int]], capacity: Int): Boolean = {
    val sorted = trips.sortBy(x => x(1))
    val init = sorted.head
    var arrive = collection.mutable.Map(init(2) -> init(0))
    var nums = init(0)
    if (nums > capacity) return false
    var res = true
    var lastArrive = init(1)
    import scala.util.control.Breaks._
    breakable {
      for (Array(up_num, from, to) <- sorted.tail) {
        arrive
          .filter(x => x._1 <= from && x._1 > lastArrive)
          .foreach(i => {
            nums = nums - i._2
            arrive.remove(i._1)
          }) //下车
        nums += up_num //上车
        val i1 = arrive.getOrElse(to, 0)
        arrive.addOne(to -> (up_num + i1)) //等下下车地点
        if (nums > capacity) {
          res = false
          break()
        }
        lastArrive = from
        println(arrive.values.sum)
      }
    }

    res
  }

  /**
   * 实现类默认实现set 方法体为 Map("实例方法"->小驼峰命名的方法)
   */
  override def set: Map[String, ((Array[Array[Int]], Int)) => Boolean] = Map("默认方法" -> carPooling)
}

/**
 * //todo  没做完
 */
object LeetCode435 extends LeetCode[Array[Array[Int]], Int] {
  override def no: Int = 435

  override def desc: String = "给定一个区间的集合 intervals ，其中 intervals[i] = [starti, endi] 。返回 需要移除区间的最小数量，使剩余区间互不重叠 。"

  override def domain(input: Array[Array[Int]]): Boolean = input.length >= 1 && input.length <= (10 ^ 5)

  override def stdIO: Seq[(Array[Array[Int]], Int)] = {
    Seq(
      (Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(1, 3)), 1),
      (Array(Array(1, 2), Array(1, 2), Array(1, 2)), 2),
      (Array(Array(1, 2), Array(2, 3)), 0)
    )
  }

  override def set: Map[String, Array[Array[Int]] => Int] = {
    Map(
      "失败了的" -> eraseOverlapIntervals1,
      "动态规划" -> dynamic,
      "贪心" -> greedy
    )
  }

  val mark = (c: Array[Int], l: Array[Int]) => {
    if (c(1) < l(0)) {
      "before"
    } else if (c(0) > l(1)) {
      "after"
    } else if (c(0) <= l(0) && c(1) >= l(1)) {
      "contains"
    } else if (c(0) > l(0) && c(1) < l(1)) {
      "contained"
    } else if (c(0) <= l(0) && c(1) < l(1)) {
      "head"
    } else if (c(0) > l(0) && c(1) >= l(1)) {
      "tail"
    }
  }

  // 减去最小的==保留最长的
  // 超出时间限制了 性能太低
  def dynamic(intervals: Array[Array[Int]]): Int = {
    if (intervals.length == 1) return 0
    val sorted = intervals.sortBy(x => x(0))
    val notOverlap = collection.mutable.Buffer.empty[Int]
    val res = Array.fill(sorted.length)(1)
    for (i <- sorted.indices) {
      for (j <- (0 until i)) {
        if (sorted(i)(0) >= sorted(j)(1)) {
          res(i) = Math.max(res(i), res(j) + 1)
        }
      }
    }
    intervals.length - res.last
  }

  def greedy(intervals: Array[Array[Int]]): Int = {
    var res = 0
    val sorted = intervals.sortBy(x => (x(0), x(1)))
    var last = sorted.head
    for (current <- sorted.tail) {
      if (last(1) > current(1)) {
        last = current
      }
      if (current(0) >= last(1)) {
        res += 1
        last = current
      }
    }

    intervals.length - res - 1
  }

  def eraseOverlapIntervals1(intervals: Array[Array[Int]]): Int = {
    if (intervals.length == 1) return 0
    val sorted = intervals.sortBy(_.head)
    var res = 0
    var last = sorted.head
    for (current <- sorted.tail) {
      val flag = mark(current, last)
      last =
        flag match {
          case "before" => println("不可能")
            last
          case "after" => current
          case "head" => println("不可能")
            res + 1
            current
          case "tail" =>
            res + 1
            last
          case "contains" =>
            res + 1
            last
          case "contained" =>
            res + 1
            current
        }
    }


    res
  }
}

object LeetCode646 extends LeetCode[Array[Array[Int]], Int] {
  override def no: Int = 646

  override def desc: String = "给你一个由 n 个数对组成的数对数组 pairs ，其中 pairs[i] = [lefti, righti] 且 lefti < righti 。现在，我们定义一种 跟随 关系，当且仅当 b < c 时，数对 p2 = [c, d] 才可以跟在 p1 = [a, b] 后面。我们用这种形式来构造 数对链 。找出并返回能够形成的 最长数对链的长度 。你不需要用到所有的数对，你可以以任何顺序选择其中的一些数对来构造。"

  override def domain(input: Array[Array[Int]]): Boolean = input.length >= 1 && input.length <= 1000

  override def stdIO: Seq[(Array[Array[Int]], Int)] = {
    Seq(
      //      (Array(Array(1, 2), Array(2, 3), Array(3, 4)), 2),
      //      (Array(Array(1, 2), Array(7, 8), Array(4, 5)), 3),
      (Array(Array(1, 2), Array(6, 7), Array(4, 5), Array(3, 6), Array(2, 10), Array(3, 8)), 3)
    )
  }

  override def set: Map[String, Array[Array[Int]] => Int] = {
    Map(
      "贪心" -> findLongestChain,
      "动态规划->" -> dynamic
    )
  }

  val order: Ordering[Array[Int]] = new Ordering[Array[Int]] {
    override def compare(x: Array[Int], y: Array[Int]): Int = {
      if (x(0) > y(0)) {
        +1
      } else if (x(0) < y(0)) {
        -1
      } else {
        x(1) - y(1)
      }
    }
  }

  def dynamic(pairs: Array[Array[Int]]): Int = {
    // 将所有的数对按照开始时间进行排序
    val sorted = pairs.sortBy(x => (x(0)))
    println(sorted.map(x => x: String): String)
    // 初始化动态规划数组，dp[i] 表示到第 i 个数对为止能够形成的最长数对链的长度
    val dp = Array.fill(sorted.length)(1)
    // 遍历所有的数对
    for (i <- sorted.indices) {
      // 对于每个数对，检查所有在它之前的数对
      for (j <- (0 until i)) {
        // 如果当前数对的开始时间大于前一个数对的结束时间
        if (sorted(i)(0) > sorted(j)(1)) {
          // 那么就可以将当前数对添加到数对链中，更新 dp 数组
          dp(i) = Math.max(dp(i), dp(j) + 1)
        }
      }
    }
    // 返回 dp 数组的最后一个元素，即最长数对链的长度
    dp(dp.length - 1)
  }


  def findLongestChain(pairs: Array[Array[Int]]): Int = {
    val sorted = pairs.sortBy(x => (x(0), x(1)))
    val res = collection.mutable.Buffer.empty[Array[Int]]
    var last = sorted.head
    for (current <- sorted.tail) {
      if (current(1) < last(1)) {
        last = current
      }
      if (current(0) > last(1)) {
        res.append(last)
        last = current
      }
    }
    res.append(last)
    res.length
  }
}

