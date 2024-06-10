package org.eterk.klearn.untyped

object IsomorphicString205 extends LeetCode[(String, String), Boolean] {

  override def no: Int = 205

  override def desc: String = "给定两个字符串 s 和 t ，判断它们是否是同构的。如果 s 中的字符可以按某种映射关系替换得到 t ，那么这两个字符串是同构的。每个出现的字符都应当映射到另一个字符，同时不改变字符的顺序。不同字符不能映射到同一个字符上，相同字符只能映射到同一个字符上，字符可以映射到自己本身。"

  override def domain(input: (String, String)): Boolean = {
    val (s, t) = input
    1 <= s.length && s.length <= 5 * 104 && t.length == s.length
  }

  override def stdIO: Seq[((String, String), Boolean)] = Seq(
    (("egg", "add"), true),
    (("foo", "bar"), false),
    (("badc", "baba"), false),
    (("paper", "title"), true)
  )

  def isIsomorphic(s: String, t: String): Boolean = {
    var res = true
    if (s.length != t.length) {
      return false
    }
    import scala.util.control.Breaks._
    val map = collection.mutable.Map.empty[Char, Char]
    val map2 = collection.mutable.Map.empty[Char, Char]
    breakable {
      for ((i, j) <- s.zip(t)) {
        //处理映射不一致的
        val flag1 = map.isDefinedAt(i) && map(i) != j
        // 标记不同字符映射到同一个字符的
        val flag2 = !map.isDefinedAt(i) && map2.isDefinedAt(j)

        if (flag1 || flag2) {
          return false
        }
        map(i) = j
        map2(j) = j

      }
    }
    res
  }

  //明确了输入是由小写字母组成的，所以我们可以用一个长度为26的数组来代替哈希映射
  def isIsomorphic1(s: String, t: String): Boolean = {
    if (s.length != t.length) {
      return false
    }
    val map1 = new Array[Int](s.length)
    val map2 = new Array[Int](s.length)
    for (i <- s.indices) {
      val index1 = s(i) - 'a'
      val index2 = t(i) - 'a'
      if (map1(index1) != map2(index2)) {
        return false
      }
      map1(index1) = i + 1
      map2(index2) = i + 1
    }
    true
  }


  override def set: Map[String, ((String, String)) => Boolean] = Map("哈希表" -> (isIsomorphic _).tupled)
}