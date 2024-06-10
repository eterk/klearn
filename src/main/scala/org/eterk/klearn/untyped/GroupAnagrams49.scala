package org.eterk.klearn.untyped

object GroupAnagrams49 extends LeetCode[Array[String], List[List[String]]] {

  override def no: Int = 49

  override def desc: String = "给你一个字符串数组，请你将 字母异位词 组合在一起。可以按任意顺序返回结果列表。字母异位词 是由重新排列源单词的所有字母得到的一个新单词。"

  override def domain(input: Array[String]): Boolean = {
    1 <= input.length && input.length <= 104 && input.forall(str => 0 <= str.length && str.length <= 100)
  }

  override def stdIO: Seq[(Array[String], List[List[String]])] = Seq(
    ((Array("eat", "tea", "tan", "ate", "nat", "bat")), List(List("eat", "tea", "ate"), List("tan", "nat"), List("bat"))),
    ((Array("")), List(List(""))),
    ((Array("b", " ")), List(List("b"), List(""))),
    ((Array("a")), List(List("a")))
  )

  override def set: Map[String, Array[String] => List[List[String]]] = Map("哈希表" -> groupAnagrams)

  import scala.util.control.Breaks._

  def isAnagrams(str: String, target: String): Boolean = {
    val map = collection.mutable.Map.empty[Char, Int]
    var res = true
    for (i <- str) {
      if (map.isDefinedAt(i)) {
        map.put(i, map(i) + 1)
      } else {
        map.put(i, 1)
      }
    }
    breakable {
      for (i <- target) {
        if (map.isDefinedAt(i)) {
          if (map(i) == 1) {
            map.remove(i)
          } else {
            map.update(i, map(i) - 1)
          }
        } else {
          res = false
          break()
        }
      }
    }

    res
  }

  // todo  没通过测试
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    if (strs.isEmpty) return List.empty
    if (strs.length == 1) return List(strs.toList)
    val map = collection.mutable.LinkedHashMap[String, List[String]](strs.head -> Nil)

    for (str <- strs.tail) {
      map.keySet.find(key => {
        isAnagrams(str, key)
      }) match {
        case Some(f) => map.update(f, map(f) :+ str)
        case None => map.update(str, Nil)
      }


    }

    map.map {
      case (k, v) => k +: v
    }.toList
  }
}