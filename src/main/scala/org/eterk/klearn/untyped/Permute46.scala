package org.eterk.klearn.untyped

/**
 * 回溯算法：
 * 回溯算法是一种通过探索所有可能的候选解来找出所有的解的算法。如果候选解被确认不是一个解（或者至少不是最后一个解），回溯算法会通过在上一步进行一些变化来舍弃该解，即“回溯”并且尝试另一种可能。
 *
 * 回溯算法的典型应用包括：八皇后问题、图的着色、旅行商问题等。
 *
 * 以下是回溯算法的基本步骤：
 *
 * 从根节点开始，这里的“根”是指一个没有任何决策的空集。
 * 令我们的程序在每一步都检查我们是否找到了一个解。如果我们找到了一个解，那么我们的算法将会立即结束。
 * 然后向左走。如果这个节点无法访问，那么就向右走。
 * 如果这个节点是叶子，那么就向上走，并删除这个叶子。
 * 重复步骤 3 和 4 直到找到解或者遍历完整棵树。
 * 深度优先搜索算法：
 *
 * 深度优先搜索（DFS）是一种用于遍历或搜索树或图的算法。这个算法会尽可能深的搜索树的分支。当节点v的所在边都己被探寻过，搜索将回溯到发现节点v的那条边的起始节点。这一过程一直进行到已发现从源节点可达的所有节点为止。如果还存在未被发现的节点，则选择其中一个作为源节点并重复以上过程，整个进程反复进行直到所有节点都被访问为止。
 *
 * 以下是深度优先搜索算法的基本步骤：
 *
 * 从根节点开始，选择一条路径尽可能深地搜索，直到这条路径的最后一个节点。
 * 如果这个节点没有子节点，那么回溯到上一个节点。
 * 如果有未访问的子节点，那么选择一个未访问的子节点，重复步骤 1。
 * 如果所有的节点都被访问过，那么算法结束。
 *
 */
object Permute46 {

  //  Given an array nums of distinct integers, return all the possible permutations. You can return the answer in any order.
  def apply(nums: Array[Int]): List[List[Int]] = {

    s2(nums)
  }
  // todo 这道回溯我完全不会
  def s1(nums: Array[Int]): List[List[Int]] = {
    nums.toList match {
      case Nil => List(Nil)
      case _ => for {
        i <- nums.indices.toList
        rest = nums.take(i) ++ nums.drop(i + 1)
        p <- s1(rest)
      } yield nums(i) :: p
    }

  }

  import scala.collection.mutable

  object s2 {
    def apply(nums: Array[Int]): List[List[Int]] = {
      val results = new mutable.ListBuffer[List[Int]]
      backtrack(nums, results, new mutable.ListBuffer[Int])
      results.toList
    }

    private def backtrack(nums: Array[Int],
                          results: mutable.ListBuffer[List[Int]],
                          tempList: mutable.ListBuffer[Int]): Unit = {
      if (tempList.length == nums.length) {
        println("+ " + tempList + ";")
        println()
        results += tempList.toList
      } else {
        for (i <- nums.indices) {

          if (!tempList.contains(nums(i))) {
            println(tempList)
            tempList += nums(i)
            backtrack(nums, results, tempList)
            tempList.remove(tempList.size - 1) // 进行回溯
          }

        }
      }
    }
  }


  def main(args: Array[String]): Unit = {

    apply(Array(1, 2, 3)).foreach(println)

  }


}
