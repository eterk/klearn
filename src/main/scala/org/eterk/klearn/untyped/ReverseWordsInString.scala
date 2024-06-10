package org.eterk.klearn.untyped

object ReverseWordsInString extends LeetCode[String, String] {

  override def no: Int = 151

  override def domain(input: String): Boolean = {
    input.length >= 1 && input.length <= 104 && input.exists(_.isLetterOrDigit)
  }

  override def stdIO: Seq[(String, String)] = Seq(
    ("the sky is blue", "blue is sky the"),
    ("  hello world  ", "world hello"),
    ("a good   example", "example good a")
  )

  override def set: Map[String, String => String] = Map(
    "原生" -> reverseWords,
    "栈" -> reverse,
    "栈" -> reverse1
  )

  override def current: Option[String => String] = set.get("栈")

  def reverseWords(s: String): String = {
    s.split(" ").filterNot(x => x == " " || x == "").reverse.mkString(" ")
  }


  def reverse(s: String): String = {
    val stack = collection.mutable.Stack[String]()
    val current = new StringBuilder()
    val iter = s.iterator
    while (iter.hasNext) {
      val charNow = iter.next()

      if (charNow != ' ') {
        current += charNow
      } else {
        if (current.nonEmpty) {
          stack.push(current.toString())
          current.clear()
        }
      }
    }

    if (current.nonEmpty) {
      stack.push(current.toString())
    }

    stack.popAll().mkString(" ")

  }

  def reverse1(s: String): String = {
    val stack = collection.mutable.Stack[String]()
    s.split(' ').foreach { word =>
      if (word.nonEmpty) {
        stack.push(word)
      }
    }
    stack.popAll().mkString(" ")
  }

}
