package org.eterk.klearn

import scala.language.implicitConversions

abstract class LeetCode[T, U] extends LeetCodeDesc[T, U] with (T => U) {
  implicit def f2Tof1[A, B, C](f: (A, B) => C): Function[(A, B), C] = {
    f.tupled
  }

  implicit def f3Tof1[A, B, C, D](f: (A, B, C) => D): Function[(A, B, C), D] = {
    f.tupled
  }

  implicit def f4Tof1[A, B, C, D, O](f: (A, B, C, D) => O): Function[(A, B, C, D), O] = {
    f.tupled
  }

  override def desc: String = ""

  def stdTest(): Unit = {
    val res = stdTestRes()
    val right = res.forall(_._4)
    val head = s"\n$no.  ${getClass.getSimpleName}   : $right \n"
    println(s"$head${res.mkString("\n")}")
  }

  implicit def arrayToString[T](array: Array[T]): String = array.mkString("Arr(", ",", ")")


  private def convert(i: U): Any = {
    i match {
      case value: Array[_] =>
        var res: Seq[_] = value.toSeq
        if (res.nonEmpty && res.head.isInstanceOf[Array[_]]) {
          res.map(i => i.asInstanceOf[Array[_]].toSeq)
        } else {
          res
        }

      case _ =>
        i
    }
  }

  def stdTestRes(): Seq[(String, Int, Any, Boolean)] = {

    set.flatMap {
      case (name, f: (T => U)) =>
        stdIO
          .zipWithIndex
          .map {
            case ((i, o), index) =>
              val res = convert(f(i))
              (name, index, res, res == convert(o))
          }
    }.toSeq

  }

  def current: Option[T => U] = set.headOption.map(_._2)

  override def apply(v1: T): U = {
    require(domain(v1))
    current.get(v1)
  }

}