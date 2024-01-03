package org.eter.klearn
package sorted

object QuickSort {

  def quickSortList(seq: List[Int]): List[Int] = {
    seq match {
      case Nil => Nil
      case p1 :: p2 =>
        val (less, greater) = p2.partition(_ < p1)
        quickSortList(less) ::: p1 :: quickSortList(greater)
    }
  }

  def quickSortSeq(seq: Seq[Int]): Seq[Int] = {
    seq match {
      case Nil => Nil
      case p1 +: p2 =>
        val (less, greater) = p2.partition(_ < p1)
        (quickSortSeq(less) :+ p1) ++ quickSortSeq(greater)
    }
  }



  def main(args: Array[String]): Unit = {
    println(quickSortList(List(9, 8, 7, 6, 5, 4, 3, 2, 1)).mkString(","))
    println(quickSortSeq(Seq(9, 8, 7, 6, 5, 4, 3, 2, 1)).mkString(","))

  }

}
//
//import org.scalameter._
//
//import scala.Console.in
//
//object MyBenchmark extends Bench.LocalTime {
//  val sizes = Gen.range("size")(300000, 1500000, 300000)
//
//  val ranges = for {
//    size <- sizes
//  } yield 0 until size
//
//  performance of "Range" in {
//    measure method "map" in {
//      using(ranges) in {
//        r => r.map(_ + 1)
//      }
//    }
//  }
//}
//
//
//import java.util.concurrent.TimeUnit
//import org.openjdk.jmh.annotations._
//import scala.util.Random
//
//@OutputTimeUnit(TimeUnit.MILLISECONDS)
//@BenchmarkMode(Array(Mode.AverageTime))
//@State(Scope.Thread)
//class QuickSortBenchmark {
//
//  @Param(Array("1000", "10000", "100000"))
//  var size: Int = _
//
//  var list: List[Int] = _
//  var seq: Seq[Int] = _
//
//  @Setup
//  def setup(): Unit = {
//    val data = (1 to size).map(_ => Random.nextInt()).toArray
//    list = data.toList
//    seq = data.toSeq
//  }
//
//  @Benchmark
//  def quickSortList(): Unit = {
//    QuickSort.quickSortList(list)
//  }
//
//  @Benchmark
//  def quickSortSeq(): Unit = {
//    QuickSort.quickSortSeq(seq)
//  }
//
//
//}
