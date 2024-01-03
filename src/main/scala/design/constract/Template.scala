package org.eter.klearn
package design.constract

object Template {

  // 抽象模板类
  abstract class Game {
    def initialize(): Unit

    def startPlay(): Unit

    def endPlay(): Unit

    // 模板方法
    final def play(): Unit = {
      initialize()
      startPlay()
      endPlay()
    }
  }

  // 具体模板类
  class Cricket extends Game {
    override def initialize(): Unit = println("Cricket Game Initialized! Start playing.")

    override def startPlay(): Unit = println("Cricket Game Started. Enjoy the game!")

    override def endPlay(): Unit = println("Cricket Game Finished!")
  }

  class Football extends Game {
    override def initialize(): Unit = println("Football Game Initialized! Start playing.")

    override def startPlay(): Unit = println("Football Game Started. Enjoy the game!")

    override def endPlay(): Unit = println("Football Game Finished!")
  }

  // 客户端代码
  def main(args: Array[String]): Unit = {
    var game: Game = new Cricket()
    game.play()
    println()
    game = new Football()
    game.play()
  }


}
