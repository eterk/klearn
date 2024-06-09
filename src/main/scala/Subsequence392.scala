object Subsequence392 extends LeetCode[(String, String), Boolean] {

  override def no: Int = 392

  override def desc: String = "判断子序列"

  override def domain(input: (String, String)): Boolean = {
    val (s, t) = input
    0 <= s.length && s.length <= 100 &&
      0 <= t.length && t.length <= math.pow(10, 4) &&
      s.forall(_.isLower) && t.forall(_.isLower)
  }

  override def stdIO: Seq[((String, String), Boolean)] = Seq(
    (("abc", "ahbgdc"), true),
    (("b", "c"), false),
    (("", "cdasdas"), true),
    (("axc", "ahbgdc"), false)
  )

  override def set: Map[String, ((String, String)) => Boolean] = Map("双指针" -> (isSubsequence _).tupled)

  def isSubsequence(s: String, t: String): Boolean = {

    var iterS = s.iterator
    var iterT = t.iterator
    var notFoundYet = false
    while (iterT.hasNext && iterS.hasNext) {
      notFoundYet = true
      val current = iterS.next()
      while (iterT.hasNext && notFoundYet) {
        if (current == iterT.next()) {
          notFoundYet = false
        }
      }
    }

    !iterS.hasNext && (!notFoundYet)
  }
}
