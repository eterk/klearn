object IntegerToRoman extends LeetCode[Int, String] {

  override def no: Int = 12

  override def domain(input: Int): Boolean = {
    input >= 1 && input <= 3999
  }

  override def stdIO: Seq[(Int, String)] = Seq(
    (3749, "MMMDCCXLIX"),
    (58, "LVIII"),
    (1994, "MCMXCIV"),
    (1001, "MI")
  )

  override def set: Map[String, Int => String] = Map(
    "示例解法" -> intToRoman
  )


  def intToRoman(num: Int): String = {

    val levelMap: Seq[((Int, Int), Map[Int, String])] = Seq(
      (1000, 9999) -> Map(1 -> "M",
        5 -> "V",
        10 -> "P"
      ),

      (100, 999) -> Map(1 -> "C",
        5 -> "D",
        10 -> "M"
      ),
      (10, 99) -> Map(1 -> "X",
        5 -> "L",
        10 -> "C"
      ),
      (1, 9) -> Map(1 -> "I",
        5 -> "V",
        10 -> "X"
      )

    )

    def rec(num: Int, r: String, level: Seq[((Int, Int), Map[Int, String])]): String = {
      if (level.isEmpty) return r
      val tail = level.tail
      val ((cBasic, cMax), map) = level.head
      if (cBasic <= num && num <= cMax) {
        val numL: Int = num / cBasic

        val newPart: String =
          numL match {
            case 4 =>
              map(1) + map(5)
            case 9 =>
              map(1) + map(10)
            case 5 => map(5)
            case other =>
              if (other > 5) {
                map(5) + Array.fill(other - 5)(map(1)).mkString("")
              } else {
                Array.fill(other)(map(1)).mkString("")
              }
          }

        rec(num - numL * cBasic, r ++ newPart, tail)

      } else {
        rec(num, r, tail)
      }
    }

    rec(num, "", levelMap)
  }
}
