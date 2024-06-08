
trait LeetCode[T, U] extends (T => U) {

  /**
   * 题目的序号
   */
  def no: Int


  def domain(input: T): Boolean

  /**
   * 标准的输入输出
   */
  def stdIO: Seq[(T, U)]

  def stdTest(): Seq[(String, Int, Boolean)] = {

    set.flatMap {
      case (name, f) =>
        stdIO
          .zipWithIndex
          .map {
            case ((i, o), index) => (name, index, f(i) == o)
          }
    }.toSeq

  }

  def set: Map[String, T => U]

  def current: Option[T => U] = set.headOption.map(_._2)

  override def apply(v1: T): U = {
    require(domain(v1))
    current.get(v1)
  }

}