package org.eter.klearn
package queue

import scala.collection.mutable

object MyQueue232 {

  //在做此题时我一心想在push 阶段完成整个队列（把新加入的元素移动到栈低）的功能，后来发现 可以在其他部分进行后续操作
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
