package org.eterk.klearn

trait LeetCode[T, U] extends LeetCodeDesc[T, U] with (T => U) {

  override def desc: String = ""

  def stdTest(): Seq[(String, Int, U, Boolean)] = {

    set.flatMap {
      case (name, f) =>
        stdIO
          .zipWithIndex
          .map {
            case ((i, o), index) =>
              val res = f(i)
              (name, index, res, res == o)
          }
    }.toSeq

  }

  def current: Option[T => U] = set.headOption.map(_._2)

  override def apply(v1: T): U = {
    require(domain(v1))
    current.get(v1)
  }

  def set: Map[String, T => U]

}
