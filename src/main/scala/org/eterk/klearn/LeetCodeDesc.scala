package org.eterk.klearn


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