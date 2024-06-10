package org.eter.klearn
package sorted

object LemonadeChange860 {

  def s1(bills: Array[Int]): Boolean = {

    var i = 0
    val myMoney = collection.mutable.Map[Int, Int](5 -> 0, 10 -> 0, 20 -> 0)
    val receive = (money: Int) => myMoney.update(money, myMoney(money) + 1)
    val send = (money: Int, nums: Int) => myMoney.update(money, myMoney(money) - nums)

    while (i < bills.length) {
      val m = bills(i)
      m match {
        case 5 =>
        case 10 => if (myMoney(5) >= 1) {
          send(5, 1)
        } else {
          return false
        }
        case 20 =>
          if (myMoney(10) >= 1 && myMoney(5) >= 1) {
            send(10, 1)
            send(5, 1)
          } else if (myMoney(5) >= 3) {
            send(5, 3)
          } else {
            return false
          }
      }
      receive(m)
      i += 1
    }


    true
  }

  def s2(bills: Array[Int]): Boolean = {
    var five = 0
    var ten = 0
    for (i <- bills) {
      if (i == 5) five += 1
      else if (i == 10) {
        five -= 1
        ten += 1
      }
      else if (ten > 0) {
        ten -= 1
        five -= 1
      }
      else five -= 3
      if (five < 0) return false
    }
    true
  }

  def apply(bills: Array[Int]): Boolean = {
    s2(bills)

  }

  def main(args: Array[String]): Unit = {

    println(apply(Array(5, 5, 10, 10, 20)))
    println(apply(Array(5, 5, 5, 10, 20)))
  }

}
