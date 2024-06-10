package org.eter.klearn
package queue

import scala.collection.mutable

object MyQueue232 {

  //在做此题时我一心想在push 阶段完成整个队列（把新加入的元素移动到栈低）的功能，后来发现 可以在其他部分进行后续操作
  /**
   * 整个设计原理类似于两桶空羽毛球桶（s1,s2） ，不断给羽毛球桶里放入羽毛球，为了每次能拿到最早放入的羽毛球
   * push 放羽毛球时，把羽毛球都放在(push)s1,
   * pop  取羽毛球时，先检查 s2 有没有球，如果没有球，就检查s1 有没有球，s1 ,有球的话，就把s1的 倒入s2, 然后取走s2最上面的
   * top 逻辑和 pop 类似，只是看看
   * empty s1 s2 都为空就是true
   * size s1 和 s2 的羽毛球数之和
   */
  class MyQueue() {

    private val s1 = scala.collection.mutable.Stack.empty[Int]
    private val s2 = scala.collection.mutable.Stack.empty[Int]


    def push(x: Int): Unit = {
      s1.push(x)
    }

    def pop(): Int = {
      if (s2.nonEmpty) s2.pop
      else if (s1.isEmpty) -1
      else {
        move
        s2.pop
      }
    }

    def peek(): Int = {
      if (s2.nonEmpty) s2.top
      else if (s1.isEmpty) -1
      else {
        move
        s2.top
      }
    }

    def size(): Int = s1.size + s2.size

    def empty(): Boolean = s1.isEmpty && s2.isEmpty

    private def move = {
      while (s1.nonEmpty) {
        s2.push(s1.pop)
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val q = new MyQueue()
    println(q.empty())
    q.push(1)
    println(q.peek())
    println(q.empty())
    q.push(2)
    q.push(3)
    q.push(4)
    println(q.empty())
    while (!q.empty()) {
      println(q.pop())
    }


  }


}
