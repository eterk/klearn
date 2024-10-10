package org.eterk.klearn.untyped

object CombinationSum39 extends LeetCode[(Array[Int], Int), List[List[Int]]] {
  override def no: Int = 39

  override def desc: String = "给定一个无重复元素的数组 candidates 和一个目标数 target，找出 candidates 中所有可以使数字和为 target 的组合。candidates 中的数字可以无限制重复被选取。"

  override def domain(input: (Array[Int], Int)): Boolean = input._1.length >= 1 && input._1.length <= 30 && input._2 >= 1 && input._2 <= 40

  override def stdIO: Seq[((Array[Int], Int), List[List[Int]])] = {
    Seq(
      ((Array(2, 3, 6, 7), 7), List(List(2, 2, 3), List(7))),
      ((Array(2, 3, 5), 8), List(List(2, 2, 2, 2), List(2, 3, 3), List(3, 5))),
      ((Array(2), 1), List())
    )
  }

  override def set: Map[String, ((Array[Int], Int)) => List[List[Int]]] = {
    Map(
      "默认方法" -> combinationSum,
      "回溯法" -> backtrack,
      "回溯无重复版本" -> backtrack1
    )
  }

  def combinationSum3(candidates: Array[Int], target: Int): List[List[Int]] = {
    val result = collection.mutable.Buffer.empty[List[Int]]
    val path = Vector.empty[Int]
    val can = candidates.sorted

    def back(startIndex: Int, currentPath: Vector[Int]): Unit = {
      val currentSum = currentPath.sum
      if (currentSum == target) {
        result.append(currentPath.toList)
        return
      } else if (currentSum > target) {
        return
      }
      for (index <- startIndex until can.length) {
        back(index, currentPath :+ can(index)) // 使用 :+ 操作符添加元素
      }
    }

    back(0, path)
    result.toList
  }


  def backtrack1(candidates: Array[Int], target: Int): List[List[Int]] = {
    val path = collection.mutable.Buffer.empty[Int]
    val result = collection.mutable.Buffer.empty[List[Int]]
    val can = candidates.sorted

    def back(startIndex: Int): Unit = {
      if (path.sum == target) {
        result.append(path.toList)
        return
      } else if (path.sum > target) {
        return
      }
      for (index <- startIndex until can.length) {
        path.append(can(index))
        back(index)
        path.remove(path.length - 1)
      }
    }

    back(0)
    result.toList
  }


  def backtrack(candidates: Array[Int], target: Int): List[List[Int]] = {
    val path = collection.mutable.Buffer.empty[Int]
    val result = collection.mutable.Buffer.empty[List[Int]]

    def track(sum: Int): Unit = {
      if (sum == target) {
        result.append(path.toList)
        return
      } else if (sum > target) {
        return
      }
      for (elem <- candidates.sorted) {
        path.append(elem)
        track(path.sum)
        path.remove(path.length - 1)
      }
    }

    track(0)
    result.map(_.sorted).toSet.toList
  }


  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {

    val res = collection.mutable.Buffer[List[Int]]()

    val cans = candidates.sorted

    def get(sum: Int, index: Int, context: List[Int]): Unit = {

      if (sum >= target) return

      import scala.util.control.Breaks.{breakable, break}

      breakable {
        for (i <- cans.slice(index, cans.length)) {
          val newSum = sum + i

          if (newSum == target) {
            res.append(context :+ i)
            break()
          } else if (newSum < target) {
            get(newSum, cans.indexOf(i), context :+ i)
          } else {
            break()
          }
        }


      }
    }

    get(0, 0, Nil)
    res.toList
  }
}


object CombinationSum40 extends LeetCode[(Array[Int], Int), List[List[Int]]] {
  override def no: Int = 40

  override def desc: String = "给定一个数组 candidates 和一个目标数 target，找出 candidates 中所有可以使数字和为 target 的组合。candidates 中的每个数字在每个组合中只能使用一次。"

  override def domain(input: (Array[Int], Int)): Boolean = input._1.length >= 1 && input._1.length <= 100 && input._2 >= 1 && input._2 <= 50

  override def stdIO: Seq[((Array[Int], Int), List[List[Int]])] = {
    Seq(
      ((Array(10, 1, 2, 7, 6, 1, 5), 8), List(List(1, 1, 6), List(1, 2, 5), List(1, 7), List(2, 6))),
      ((Array(1, 2), 4), List()),
      ((Array(10, 1, 2, 7, 6, 1, 5), 8), List(List(1, 1, 6), List(1, 2, 5), List(2, 6), List(1, 7))),
      ((Array(2, 5, 2, 1, 2), 5), List(List(1, 2, 2), List(5)))
    )
  }

  override def set: Map[String, ((Array[Int], Int)) => List[List[Int]]] = {
    Map(
      "默认方法" -> combinationSum2,
      "回溯法" -> combinationSum
    )
  }





  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    val cans = candidates.sorted
    val result = collection.mutable.Buffer.empty[List[Int]]

    def backtrack(startIndex: Int, res: Vector[Int]): Unit = {
      val sum = res.sum
      if (sum == target) {
        result.append(res.toList)
        return
      } else if (res.sum > target) {
        return
      }
      for (i <- startIndex until cans.length) {
        if (i > startIndex && cans(i) == cans(i - 1)) {
          // do nothing
        } else {
          backtrack(i + 1, res :+ cans(i))
        }

      }
    }

    backtrack(0, Vector.empty)

    result.toList
  }

  // todo 存在bug  搜索回溯算法
  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    val cans = candidates.sorted
    import scala.util.control.Breaks.{breakable, break}

    val res = collection.mutable.Set.empty[List[Int]]

    def get(sum: Int, index: Int, current: List[Int]): Unit = {

      if (sum > target) return

      breakable {

        for (i <- cans.slice(index, cans.length)) {

          val newSum = sum + i
          if (current.isEmpty || (current.nonEmpty && i >= current.last)) {
            if (newSum > target) {
              break()

            } else if (newSum == target) {
              res.add(current :+ i)
              break()

            } else if (index < cans.length - 1) {
              println(i + " " + index + "z " + current)
              get(newSum, index + 1, current :+ i)

            }
          }
        }
      }

    }

    get(0, 0, Nil)
    res.toList
  }
}
