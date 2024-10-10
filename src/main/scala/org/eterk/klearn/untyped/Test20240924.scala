package org.eterk.klearn.untyped

import scala.annotation.tailrec

object Test20240924 extends App {

  import BinaryTree._

  val tree = BinaryTree(Seq(1, null, 2, null, null, 3))

  //  println(tree)
  val tree2 = BinaryTree(Seq(1, 2, 3, 4, 5, null, 8, null, null, 6, 7, 9))
  //  println(tree2)
  //  println(preorderTraversal(tree2))
  //  println(preorderTraversal(tree))
  //  println(postorderTraversal(tree2))
  //  println(postorderTraversal(tree))
  //  println(inorderTraversal(tree))
  //  println(stackZip(Seq(10, 20, 50, 80, 1, 1)))
  //  println(stackZip2(Seq(2, 2, 4, 8, 16, 1)))
  println(levelOrder(tree2))
  println(levelOrder1(tree2))

}


object BinaryTree {

  def apply(array: Seq[Integer]): TreeNode = {

    def rec(i: Integer): TreeNode = {
      if (array.length <= i || array(i) == null) {
        null
      } else {
        TreeNode(array(i), rec(2 * i + 1), rec(2 * i + 2))
      }
    }

    rec(0)

  }

  def postorderTraversal(root: TreeNode): List[Int] = {
    val res = collection.mutable.Buffer.empty[Int]

    def rec(tree: TreeNode): Unit = {
      if (tree != null) {
        if (tree.left != null) rec(tree.left)
        if (tree.right != null) rec(tree.right)
        res.append(tree.value)

      }
    }

    rec(root)
    res.toList
  }

  def inorderTraversal(root: TreeNode): List[Int] = {
    val res = collection.mutable.Buffer.empty[Int]

    def rec(tree: TreeNode): Unit = {
      if (tree != null) {
        if (tree.left != null) rec(tree.left)
        res.append(tree.value)
        if (tree.right != null) rec(tree.right)
      }
    }

    rec(root)
    res.toList
  }


  def preorderTraversal(root: TreeNode): List[Int] = {
    val res = collection.mutable.Buffer.empty[Int]

    def rec(tree: TreeNode): Unit = {
      if (tree != null) {
        res.append(tree.value)
        if (tree.left != null) {
          rec(tree.left)
        }
        if (tree.right != null) {
          rec(tree.right)
        }
      }


    }

    rec(root)

    res.toList

  }

  def levelOrder1(root: TreeNode): List[Int] = {
    val queue = collection.mutable.Queue.empty[Int]


    def rec(tree: TreeNode): Unit = {
      if (tree != null) {
        queue.append(tree.value)

        rec(tree.left)
        rec(tree.right)
      }

    }

    rec(root)


    queue.toList

  }


  /**
   * leetcode 102
   */
  def levelOrder(root: TreeNode): List[List[Int]] = {

    if (root == null) return List.empty

    val queue = collection.mutable.Queue(root)

    val res = collection.mutable.Buffer.empty[List[Int]]


    while (queue.nonEmpty) {
      var size = queue.size
      val tmp = collection.mutable.Buffer.empty[Int]
      while (size > 0) {
        val current = queue.dequeue()
        tmp.append(current.value)

        if (current.left != null) {
          queue.enqueue(current.left)
        }
        if (current.right != null) {
          queue.enqueue(current.right)
        }


        size -= 1
      }
      res.append(tmp.toList)


    }
    res.toList
  }

  // 102.二叉树的层序遍历

  import scala.collection.mutable

  def levelOrder2(root: TreeNode): List[List[Int]] = {
    val res = mutable.ListBuffer[List[Int]]()
    if (root == null) return res.toList
    val queue = mutable.Queue[TreeNode]() // 声明一个队列
    queue.enqueue(root) // 把根节点加入queue
    while (!queue.isEmpty) {
      val tmp = mutable.ListBuffer[Int]()
      val len = queue.size // 求出len的长度
      for (i <- 0 until len) { // 从0到当前队列长度的所有节点都加入到结果集
        val curNode = queue.dequeue()
        tmp.append(curNode.value)
        if (curNode.left != null) queue.enqueue(curNode.left)
        if (curNode.right != null) queue.enqueue(curNode.right)
      }
      res.append(tmp.toList)
    }
    res.toList
  }


  def stackZip2(nums: Seq[Int]): Unit = {
    var stack = collection.mutable.Stack.empty[Int]

    def push(num: Int): Unit = {
      var total = num
      var i = stack.length - 1
      while (i >= 0 && total > 0) {
        total -= stack(i)
        if (total == 0) {
          stack.trimEnd(stack.length - i)
          push(num * 2)
          return
        }
        i -= 1
      }
      stack.push(num)
    }

    nums.foreach(push)
    println(stack.reverse.mkString(" "))
  }


  def stackZip(seq: Seq[Int]): Seq[Int] = {
    val stack = collection.mutable.Stack.empty[Int]

    @tailrec
    def rec(value: Int, sum: Int, buffer: Seq[Int]): Unit = {
      if (sum == value) {
        stack.push(2 * value)
      } else if (sum < value && stack.nonEmpty) {
        val v = stack.pop()
        rec(value, sum + v, v +: buffer)
      } else {
        buffer.foreach(v => stack.push(v))
      }
    }

    val iter = seq.iterator
    while (iter.hasNext) {
      val value = iter.next()
      rec(value, 0, Seq(value))
    }

    stack.toSeq

  }

}
