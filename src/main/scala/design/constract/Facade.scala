package org.eter.klearn
package design.constract


// 客户端代码
object Facade {

  // 子系统类
  class CPU {
    def freeze(): Unit = println("CPU freeze")

    def jump(position: Long): Unit = println(s"CPU jump to $position")

    def execute(): Unit = println("CPU execute")
  }

  class Memory {
    def load(position: Long, data: Array[Byte]): Unit =
      println(s"Memory load from $position data: ${data.mkString}")
  }

  class HardDrive {
    def read(lba: Long, size: Int): Array[Byte] = {
      println(s"HardDrive read lba: $lba size: $size")
      Array(1, 2, 3)
    }
  }

  // 外观类
  class ComputerFacade {
    private val processor = new CPU()
    private val ram = new Memory()
    private val hd = new HardDrive()

    def start(): Unit = {
      processor.freeze()
      val bootAddress = 0L
      val bootSector = hd.read(bootAddress, 512)
      ram.load(bootAddress, bootSector)
      processor.jump(bootAddress)
      processor.execute()
    }
  }

  def main(args: Array[String]): Unit = {
    val computer = new ComputerFacade()
    computer.start()
  }
}

