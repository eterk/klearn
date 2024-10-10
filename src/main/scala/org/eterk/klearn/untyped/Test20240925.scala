package org.eterk.klearn.untyped


object Test20240925 extends App {

  import Dynamic._

  //  println(message(6, Seq(10, 20, 30, 40, 60)))

  println(message(6, Seq(30, 10, 20, 50, 60)))


  println(jump(2, Seq(1, -1, -6, 7, -17, 7)))

}


object Dynamic {

  def message(budget: Int, prices: Seq[Int]): Int = {
    val dp = Array.fill(budget + 1)(0)

    for (i <- 1 to prices.length) {
      for (j <- i to budget) {
        println(dp.toSeq)
        dp(j) = math.max(dp(j), dp(j - i) + prices(i - 1))
      }
    }

    dp(budget)
  }

  def jump(k: Int, score: Seq[Int]): Int = {
    val dp = Array.fill(score.length)(0)
    dp(0) = score(0) // 初始化起点得分

    for (i <- 1 until dp.length) {
      dp(i) = score(i) // 初始化 dp(i) 为 score(i)
      for (j <- Math.max(i - k, 0) until i) {
        dp(i) = Math.max(dp(j) + score(i), dp(i))
      }
    }

    dp.last
  }


}
