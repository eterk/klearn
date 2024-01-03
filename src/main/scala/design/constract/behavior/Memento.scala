package org.eter.klearn
package design.constract.behavior

object Memento {

  // 备忘录类
  case class Memento(state: String)

  // 发起人类
  class Originator {
    private var state: String = _

    def setState(state: String): Unit = this.state = state

    def getState: String = state

    def saveStateToMemento(): Memento = Memento(state)

    def getStateFromMemento(memento: Memento): Unit = state = memento.state
  }

  // 管理者类
  class CareTaker {
    private val mementoList = new java.util.ArrayList[Memento]()

    def add(state: Memento): Unit = mementoList.add(state)

    def get(index: Int): Memento = mementoList.get(index)
  }

  // 客户端代码

  def main(args: Array[String]): Unit = {
    val originator = new Originator()
    val careTaker = new CareTaker()

    originator.setState("State #1")
    originator.setState("State #2")
    careTaker.add(originator.saveStateToMemento())

    originator.setState("State #3")
    careTaker.add(originator.saveStateToMemento())

    originator.setState("State #4")
    println(s"Current State: ${originator.getState}")

    originator.getStateFromMemento(careTaker.get(0))
    println(s"First saved State: ${originator.getState}")
    originator.getStateFromMemento(careTaker.get(1))
    println(s"Second saved State: ${originator.getState}")
  }


}
