package org.eterk.klearn.untyped

object CanConstruct383 extends LeetCode[(String, String), Boolean] {

  override def no: Int = 383

  override def desc: String = "给你两个字符串：ransomNote 和 magazine ，判断 ransomNote 能不能由 magazine 里面的字符构成。如果可以，返回 true ；否则返回 false 。magazine 中的每个字符只能在 ransomNote 中使用一次。"

  override def domain(input: (String, String)): Boolean = {
    val (ransomNote, magazine) = input
    1 <= ransomNote.length && ransomNote.length <= 105 && 1 <= magazine.length && magazine.length <= 105
  }

  override def stdIO: Seq[((String, String), Boolean)] = Seq(
    (("a", "b"), false),
    (("aa", "ab"), false),
    (("aa", "aab"), true)
  )


  def canConstruct(ransomNote: String, magazine: String): Boolean = {
    import scala.util.control.Breaks._
    val map =
      magazine.foldLeft(collection.mutable.Map.empty[Char, Int])((map, c) => {
        if (map.isDefinedAt(c)) {
          map(c) += 1
        } else {
          map.update(c, 1)
        }
        map
      })
    var res = true
    breakable {
      for (c <- ransomNote) {
        if (map.isDefinedAt(c)) {
          if (map(c) == 1) {
            map.remove(c)
          } else {
            map(c) -= 1
          }
        } else {
          res = false
          break()

        }
      }
    }

    res
  }

  override def set: Map[String, ((String, String)) => Boolean] = Map("哈希表" -> (canConstruct _).tupled)
}
