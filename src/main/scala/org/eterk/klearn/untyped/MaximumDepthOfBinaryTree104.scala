package org.eterk.klearn.untyped

import org.eterk.klearn.untyped

import scala.annotation.tailrec

object MaximumDepthOfBinaryTree104 extends LeetCode[TreeNode, Int] {
  override def no: Int = 104

  override def desc: String = "给定一个二叉树，找出其最大深度。二叉树的深度为根节点到最远叶子节点的最长路径上的节点数。"

  override def domain(input: TreeNode): Boolean = input != null

  override def stdIO: Seq[(TreeNode, Int)] = {
    Seq(
      (TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7))), 3),
      (TreeNode(1, null, TreeNode(2)), 2),
      (null, 0)
    )
  }

  override def set: Map[String, TreeNode => Int] = {
    Map(
      "默认方法" -> maxDepth
    )
  }

  def maxDepth(root: TreeNode): Int = {
    var max = 0


    def find(current: TreeNode, base: Int): Unit = {
      if (current == null) {
        if (base > max) {
          max = base
        }

      } else {
        find(current.left, base + 1)
        find(current.right, base + 1)
      }


    }

    find(root, 0)
    max


  }
}

object SameTree100 extends LeetCode[(TreeNode, TreeNode), Boolean] {
  override def no: Int = 100

  override def desc: String = "给定两个二叉树，编写一个函数来检验它们是否相同。"

  override def domain(input: (TreeNode, TreeNode)): Boolean = true

  override def stdIO: Seq[((TreeNode, TreeNode), Boolean)] = {
    Seq(
      ((TreeNode(1, TreeNode(2), TreeNode(3)), TreeNode(1, TreeNode(2), TreeNode(3))), true),
      ((TreeNode(1, TreeNode(2)), TreeNode(1, null, TreeNode(2))), false),
      ((TreeNode(1, TreeNode(2), TreeNode(1)), TreeNode(1, TreeNode(1), TreeNode(2))), false)
    )
  }

  override def set: Map[String, ((TreeNode, TreeNode)) => Boolean] = {
    Map(
      "默认方法" -> isSameTree
    )
  }

  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    if (p == null ^ q == null) {
      return false
    } else if (p == null && q == null) {
      return true
    }
    if (p.value != q.value || !isSameTree(p.left, q.left) || !isSameTree(p.right, q.right)) return false


    true
  }
}

object InvertBinaryTree226 extends LeetCode[TreeNode, TreeNode] {
  override def no: Int = 226

  override def desc: String = "翻转一棵二叉树。"

  override def domain(input: TreeNode): Boolean = input != null

  override def stdIO: Seq[(TreeNode, TreeNode)] = {
    Seq(
      (TreeNode(4, TreeNode(2, TreeNode(1), TreeNode(3)), TreeNode(7, TreeNode(6), TreeNode(9))),
        TreeNode(4, TreeNode(7, TreeNode(9), TreeNode(6)), TreeNode(2, TreeNode(3), TreeNode(1)))),
      (TreeNode(2, TreeNode(1), TreeNode(3)), TreeNode(2, TreeNode(3), TreeNode(1))),
      (null, null)
    )
  }

  override def set: Map[String, TreeNode => TreeNode] = {
    Map(
      "默认方法" -> invertTree
    )
  }

  def invertTree(root: TreeNode): TreeNode = {

    if (root != null) {
      var tmp = root.left
      root.left = root.right
      root.right = tmp
      invertTree(root.left)
      invertTree(root.right)
    }
    root

  }
}

object SymmetricTree101 extends LeetCode[TreeNode, Boolean] {
  override def no: Int = 101

  override def desc: String = "给定一个二叉树，检查它是否是镜像对称的。"

  override def domain(input: TreeNode): Boolean = input != null

  override def stdIO: Seq[(TreeNode, Boolean)] = {
    Seq(
      (TreeNode(1, TreeNode(2, TreeNode(3), TreeNode(4)), TreeNode(2, TreeNode(4), TreeNode(3))), true),


      (TreeNode(1, TreeNode(2, null, TreeNode(3)), TreeNode(2, null, TreeNode(3))), false),
      (null, true)
    )
  }

  override def set: Map[String, TreeNode => Boolean] = {
    Map(
      "默认方法" -> isSymmetric
    )
  }

  def isSymmetric(root: TreeNode): Boolean = {


    def isSame(left: TreeNode, right: TreeNode): Boolean = {

      if (left == null ^ right == null) {
        false
      } else if (left == null && right == null) {
        true
      } else {
        left.value == right.value && isSame(left.right, right.left) && isSame(left.left, right.right)
      }


    }


    isSame(root, root)


  }
}

object BuildTree105 extends LeetCode[(Array[Int], Array[Int]), TreeNode] {
  override def no: Int = 105

  override def desc: String = "从前序与中序遍历序列构造二叉树。"

  override def domain(input: (Array[Int], Array[Int])): Boolean = input._1.length == input._2.length && input._1.length >= 1 && input._1.length <= 3000

  override def stdIO: Seq[((Array[Int], Array[Int]), TreeNode)] = {
    Seq(
      ((Array(3, 9, 20, 15, 7), Array(9, 3, 15, 20, 7)), TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7)))),
      ((Array(-1), Array(-1)), TreeNode(-1))
    )
  }

  override def set: Map[String, ((Array[Int], Array[Int])) => TreeNode] = {
    Map(
      "默认方法" -> buildTree1
    )
  }

  // 这个函数在leetcode 中因为性能问题通不过?
  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    val valueIndex: Map[Int, Int] = inorder.zipWithIndex.toMap


    def build(pre: (Int, Int), in: (Int, Int)): TreeNode = {

      if (pre._1 > pre._2) return null

      val root = preorder(pre._1)

      val inRootIndex = valueIndex(root)
      val leftLen = inRootIndex - in._1

      new TreeNode(root,
        build((pre._1 + 1, pre._1 + leftLen), (in._1, inRootIndex - 1)),
        build((pre._1 + leftLen + 1, pre._2), (inRootIndex + 1, in._2)))
    }

    build((0, preorder.length - 1), (0, preorder.length - 1))
  }


  def buildTree1(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    var indexMap: Map[Int, Int] = Map()

    def myBuildTree(preorder: Array[Int], inorder: Array[Int], preorderLeft: Int, preorderRight: Int, inorderLeft: Int, inorderRight: Int): TreeNode = {
      if (preorderLeft > preorderRight) {
        return null
      }

      // 前序遍历中的第一个节点就是根节点
      val preorderRoot = preorderLeft
      // 在中序遍历中定位根节点
      val inorderRoot = indexMap(preorder(preorderRoot))

      // 先把根节点建立出来
      val root = new TreeNode(preorder(preorderRoot))
      // 得到左子树中的节点数目
      val sizeLeftSubtree = inorderRoot - inorderLeft
      // 递归地构造左子树，并连接到根节点
      // 先序遍历中「从 左边界+1 开始的 size_left_subtree」个元素就对应了中序遍历中「从 左边界 开始到 根节点定位-1」的元素
      root.left = myBuildTree(preorder, inorder, preorderLeft + 1, preorderLeft + sizeLeftSubtree, inorderLeft, inorderRoot - 1)
      // 递归地构造右子树，并连接到根节点
      // 先序遍历中「从 左边界+1+左子树节点数目 开始到 右边界」的元素就对应了中序遍历中「从 根节点定位+1 到 右边界」的元素
      root.right = myBuildTree(preorder, inorder, preorderLeft + sizeLeftSubtree + 1, preorderRight, inorderRoot + 1, inorderRight)
      root
    }

    val n = preorder.length
    // 构造哈希映射，帮助我们快速定位根节点
    indexMap = inorder.zipWithIndex.toMap
    myBuildTree(preorder, inorder, 0, n - 1, 0, n - 1)
  }

}

object BuildTree106 extends LeetCode[(Array[Int], Array[Int]), TreeNode] {
  override def no: Int = 106

  override def desc: String = "从中序与后序遍历序列构造二叉树。"

  override def domain(input: (Array[Int], Array[Int])): Boolean = input._1.length == input._2.length && input._1.length >= 1 && input._1.length <= 3000

  override def stdIO: Seq[((Array[Int], Array[Int]), TreeNode)] = {
    Seq(
      ((Array(9, 3, 15, 20, 7), Array(9, 15, 7, 20, 3)), TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7)))),
      ((Array(-1), Array(-1)), TreeNode(-1))
    )
  }

  override def set: Map[String, ((Array[Int], Array[Int])) => TreeNode] = {
    Map(
      "默认方法" -> buildTree,
      "leetcode" -> buildTree1
    )
  }

  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
    val map = inorder.zipWithIndex.toMap


    def build(in: (Int, Int), post: (Int, Int)): TreeNode = {
      if (in._2 < in._1 || post._2 < post._1) return null

      val rootValue = postorder(post._2)
      val indexOfRootOfIn = map(rootValue)
      val leftLen = indexOfRootOfIn - in._1
      val tree = TreeNode(rootValue)
      tree.left = build((in._1, indexOfRootOfIn - 1), (post._1, post._1 + leftLen - 1))
      tree.right = build((indexOfRootOfIn + 1, in._2), (post._1 + leftLen, post._2 - 1))

      tree
    }

    build((0, inorder.length - 1), (0, postorder.length - 1))


  }


  def buildTree1(inorder: Array[Int], postorder: Array[Int]): TreeNode = {

    // 从后序遍历的最后一个元素开始
    var post_idx = postorder.length - 1

    // 建立（元素，下标）键值对的哈希表
    val idx_map = inorder.zipWithIndex.toMap

    def helper(in_left: Int, in_right: Int): TreeNode = {
      // 如果这里没有节点构造二叉树了，就结束
      if (in_left > in_right) {
        return null
      }

      // 选择 post_idx 位置的元素作为当前子树根节点
      val root_val = postorder(post_idx)
      val root = new TreeNode(root_val)

      // 根据 root 所在位置分成左右两棵子树
      val index = idx_map(root_val)

      // 下标减一
      post_idx -= 1
      // 构造右子树
      root.right = helper(index + 1, in_right)
      // 构造左子树
      root.left = helper(in_left, index - 1)
      root
    }


    helper(0, inorder.length - 1)
  }
}






// 定义二叉树节点类
case class TreeNode(value: Int, var left: TreeNode = null, var right: TreeNode = null)