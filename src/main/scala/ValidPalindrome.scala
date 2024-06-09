import scala.annotation.tailrec

object ValidPalindrome extends LeetCode[String, Boolean] {

  override def no: Int = 125

  override def desc: String =
    """
      |如果在将所有大写字符转换为小写字符、并移除所有非字母数字字符之后，短语正着读和反着读都一样。则可以认为该短语是一个 回文串 。
      |
      |字母和数字都属于字母数字字符。
      |
      |给你一个字符串 s，如果它是 回文串 ，返回 true ；否则，返回 false 。
      |""".stripMargin

  override def domain(input: String): Boolean = {
    1 <= input.length && input.length <= 2 * 105
  }


  override def stdIO: Seq[(String, Boolean)] = Seq(
    ("A man, a plan, a canal: Panama", true),
    ("race a car", false),
    (" ", true),
    ("a b a b,./a ", true),
    (",./;", true),
    ("0p", false)
  )

  def isPalindrome(s: String): Boolean = {
    var (i, j) = (0, s.length - 1)

    @tailrec
    def handle(index: Int, alpha: Int): (Int, Char) = {
      if (index < 0 || index > s.length - 1) {
        return (index, '.')
      }
      val c = s(index)
      if (c.isLetterOrDigit)
        return (index + alpha, c.toLower)

      handle(index + alpha, alpha)
    }


    while (i < j) {

      val (indexI, valueI) = handle(i, 1)
      val (indexJ, valueJ) = handle(j, -1)

      if (valueI != valueJ) return false
      i = indexI
      j = indexJ
    }


    true
  }

  override def set: Map[String, String => Boolean] = Map("双指针" -> isPalindrome)
}
