package org.eterk.klearn

/**
 * 实现类命名规范 大驼峰命名+序号数字 ,实现类均为 object 单例类
 *
 * @tparam T 多个参数时为元组
 * @tparam U
 */
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
   * 注意Seq(换行)
   * Seq(
   * 1
   * .
   * )
   */
  def stdIO: Seq[(T, U)]

  /**
   * 实现类默认实现set 方法体为 Map("实例方法"->小驼峰命名的方法)
   */
  def set: Map[String, T => U]

  /**
   * 小驼峰命名的方法
   * 实现类默认给出一个实现方法 只用给出方法声明不需要给出方法实现 方法体使用???
   */
}

object StrStr28 extends LeetCode[(String, String), Int] {
  override def no: Int = 28

  override def desc: String = "实现 strStr() 函数。给定一个 haystack 字符串和一个 needle 字符串，在 haystack 字符串中找出 needle 字符串出现的第一个位置 (从0开始)。如果不存在，则返回 -1。"

  override def domain(input: (String, String)): Boolean = true

  override def stdIO: Seq[((String, String), Int)] = {
    Seq(
      (("hello", "ll"), 2),
      (("a", "a"), 0),
      (("aaaaa", "bba"), -1)
    )
  }

  override def set: Map[String, ((String, String)) => Int] = {
    Map(
      "默认方法" -> strStr
    )
  }

  def strStr(haystack: String, needle: String): Int = {
    ???
  }
}