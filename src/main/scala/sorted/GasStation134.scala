package org.eter.klearn
package sorted


/**
 *
 * bing搜到的 的解题思路
 * 首先判断是否有解，即总的汽油量是否大于等于总的消耗量。
 * 然后从任意一个加油站开始遍历，记录当前剩余的汽油量和最小的汽油量。
 * 如果遍历完一圈后，最小的汽油量大于等于0，说明有解。
 * 最后找到最小汽油量出现时的下一个加油站作为起点，返回其索引。
 */
object GasStation134 {
  def s1(gas: Array[Int], cost: Array[Int]): Int = {
    import scala.collection.mutable.ArrayBuffer
    var num: Int = 0
    val lastIndex = gas.length - 1
    val left = new ArrayBuffer[Int]()
    while (num <= lastIndex) {
      left.append(gas(num) - cost(num))
      num += 1
    }
    num = 0

    if (left.sum < 0) return -1


    val next = (index: Int) => if (index >= lastIndex) 0 else index + 1

    var first = left.indexWhere(_ >= 0)


    while (num < left.length) {
      var cur = first
      var j = 0
      var gasLeft = 0
      while (gasLeft >= 0 && j < left.length) {
        gasLeft += left(cur)
        cur = next(cur)
        j += 1
      }

      if (gasLeft >= 0 && j >= left.length) return first
      first = next(first)
      num += 1
    }

    -1

  }

  /**
   * 代码写的稀烂，思路可以
   * 将经过每一个汽油站的净耗费计算出来，如果是同号就合并，（这样可以减少数据规模）
   * 比记录好索引的开始和结束，
   * 然后遍历这些归并后的站
   * 如果遍历一圈后累加和大于等于0 那么那个开始的位置就是车站
   */
  def s2(gas: Array[Int], cost: Array[Int]): Int = {
    import scala.collection.mutable.ArrayBuffer
    var num: Int = 1
    val lastIndex = gas.length - 1
    val left = new ArrayBuffer[(Int, Int, Int)]()
    var isNature = gas(0) - cost(0) >= 0
    var dis = gas(0) - cost(0)
    var start = 0
    var end = 0
    var current = 0

    while (num <= lastIndex) {
      current = gas(num) - cost(num)
      if (isNature && current >= 0 || (!isNature) && current < 0) {
        dis += current
        end = num
      } else {
        isNature = !isNature
        left.append((start, end, dis))
        end = num
        start = num
        dis = current
      }
      num += 1
    }
    left.append((start, end, dis))

    var first = left.indexWhere(_._3 >= 0)
    if (first == -1) return -1

    val lastI = left.length - 1

    val next = (index: Int) => if (index >= lastI) 0 else index + 1
    num = 0
    while (num <= lastI) {
      var cur = first
      var j = 0
      var gasLeft = 0
      while (gasLeft >= 0 && j < left.length) {
        gasLeft += left(cur)._3
        cur = next(cur)
        j += 1
      }

      if (gasLeft >= 0 && j >= left.length) return left(first)._1
      first = next(first)
      num += 1
    }

    -1

  }

  /**
   * 你的代码的思路是这样的：
   * 首先，你将整个环形路分成若干个区间，每个区间都有一个起点和一个剩余汽油量，如果剩余汽油量大于等于0，表示可以从这个区间的起点出发，否则表示不能。
   * 然后，你找到第一个剩余汽油量大于等于0的区间，并尝试从这个区间的起点出发，遍历所有其他区间，看是否能够回到原点。
   * 如果能够回到原点，则返回这个区间的起点作为答案；否则，从下一个剩余汽油量大于等于0的区间开始重复上述过程，直到遍历完所有可能的起点或者没有找到答案。
   * 你的代码的时间复杂度是O(n^2)，因为你需要遍历所有加油站n次，并且每次还需要遍历所有其他加油站n次。你的代码的空间复杂度是O(n)，因为你需要存储所有区间信息。
   * */
  def s3(gas: Array[Int], cost: Array[Int]): Int = {
    var start = 0
    var dis = gas(0) - cost(0)
    var num = 1
    var isNature = dis >= 0
    val left = new collection.mutable.ArrayBuffer[(Int, Int)]()
    while (num < gas.length) {
      val cur = gas(num) - cost(num)
      if ((isNature && cur >= 0) || (!isNature) && cur < 0) {

        dis += cur

      } else {
        isNature = !isNature
        left.append(start -> dis)
        start = num
        dis = cur
      }

      num += 1
    }
    left.append((start, dis))

    var first = left.indexWhere(_._2 >= 0)

    if (first == -1) return -1

    val lastI = left.length - 1

    val next = (index: Int) => if (index >= lastI) 0 else index + 1

    num = 0
    while (num <= lastI) {
      var cur = first
      var j = 0
      var gasLeft = 0
      while (gasLeft >= 0 && j < left.length) {
        gasLeft += left(cur)._2
        cur = next(cur)
        j += 1
      }

      if (gasLeft >= 0 && j >= left.length) return left(first)._1
      first = next(first)
      num += 1
    }

    -1


  }


  def s4(gas: Array[Int], cost: Array[Int]): Int = {
    // 定义总的汽油量和总的消耗量
    var totalGas = 0
    var totalCost = 0
    // 定义最小的汽油量和最小汽油量出现时的索引
    var minGas = Int.MaxValue
    var minIndex = -1
    // 遍历gas和cost数组
    for (i <- gas.indices) {
      // 更新总的汽油量和总的消耗量
      totalGas += gas(i)
      totalCost += cost(i)
      // 判断当前剩余的汽油量是否小于最小值
      if (totalGas - totalCost < minGas) {
        // 更新最小值和索引
        minGas = totalGas - totalCost
        minIndex = i
      }
    }
    // 判断是否有解并返回结果
    if (totalGas < totalCost) {
      -1
    } else {
      (minIndex + 1) % gas.length
    }
  }


  def apply(gas: Array[Int], cost: Array[Int]): Int = {

    s3(gas, cost)
  }

  def main(args: Array[String]): Unit = {

    println(apply(Array(4), Array(5)))
    //    println(apply(Array(3, 3, 4), Array(3, 4, 4)))
    //    println(apply(Array(1, 2, 3, 4, 5), Array(3, 4, 5, 1, 2)))
    //    println(apply(Array(2, 3, 4), Array(3, 4, 3)))
    //    println(apply(Array(5, 1, 2, 3, 4), Array(4, 4, 1, 5, 1)))


  }
}
