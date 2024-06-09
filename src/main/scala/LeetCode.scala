
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

trait LeetCodeDesc[T, U] {

  /**
   * 题目的序号
   */
  def no: Int

  /**
   * 题目描述
   */
  def desc: String


  /**
   * 定义域限制
   */
  def domain(input: T): Boolean

  /**
   * 标准的输入输出,示例给出的输入输出对
   */
  def stdIO: Seq[(T, U)]


}