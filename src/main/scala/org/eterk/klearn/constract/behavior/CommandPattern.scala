package org.eterk.klearn.constract.behavior

import scala.collection.mutable

object CommandPattern {
  import scala.collection.mutable.Stack

  // 命令接口
  trait Command {
    def execute()
    def undo()
  }

  // 具体命令类
  class LightOnCommand(light: Light) extends Command {
    override def execute(): Unit = {
      light.on()
    }

    override def undo(): Unit = {
      light.off()
    }
  }

  class LightOffCommand(light: Light) extends Command {
    override def execute(): Unit = {
      light.off()
    }

    override def undo(): Unit = {
      light.on()
    }
  }

  // 接收者类
  class Light {
    def on(): Unit = {
      println("Light is on")
    }

    def off(): Unit = {
      println("Light is off")
    }
  }

  // 调用者类
  class RemoteControl {
    var command: Command = _
    val commandHistory: mutable.Stack[Command] = mutable.Stack()

    def setCommand(command: Command): Unit = {
      this.command = command
    }

    def buttonWasPressed(): Unit = {
      command.execute()
      commandHistory.push(command)
    }

    def undoButtonWasPressed(): Unit = {
      if (commandHistory.nonEmpty) {
        val lastCommand = commandHistory.pop()
        lastCommand.undo()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val remoteControl = new RemoteControl
    val light = new Light

    // 创建命令对象
    val lightOnCommand = new LightOnCommand(light)
    val lightOffCommand = new LightOffCommand(light)

    // 设置命令并执行
    remoteControl.setCommand(lightOnCommand)
    remoteControl.buttonWasPressed()

    // 撤销操作
    remoteControl.undoButtonWasPressed()

    // 设置命令并执行
    remoteControl.setCommand(lightOffCommand)
    remoteControl.buttonWasPressed()

    // 撤销操作
    remoteControl.undoButtonWasPressed()
  }



}
