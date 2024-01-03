package org.eter.klearn
package design.constract

trait Decorator {
  def call(): Unit
}

class Phone extends Decorator {
  override def call(): Unit = {
    println("打电话")
  }
}

class WeChatDecorator(warp: Decorator) extends Decorator {
  override def call(): Unit = {
    warp.call()
    println("发微信")
  }
}

class EMailDecorator(warp: Decorator) extends Decorator {
  override def call(): Unit = {
    warp.call()
    println("发邮件")
  }
}

object DecoratorExample {

  def main(args: Array[String]): Unit = {
    val phone = new Phone()
    val weChatPhone = new WeChatDecorator(phone)
    val emailWeChatPhone = new EMailDecorator(weChatPhone)

    emailWeChatPhone.call()

  }
}

/**
 * bing Ai  说我的装饰器模式实现错误
 */
object ColorfulShape {


  trait ColorfulShape extends BasicShape {
    val warp: BasicShape

    override def show(): Unit
  }

  trait BasicShape {
    def show(): Unit
  }

  class Zhu extends BasicShape {
    override def show(): Unit = print("柱\n")
  }

  class Shape extends BasicShape {
    override def show(): Unit = print("形\n")
  }

  class Circle(override val warp: BasicShape) extends ColorfulShape {
    override def show(): Unit = {
      print("圆")
      warp.show()
    }

  }

  class Red(override val warp: BasicShape) extends ColorfulShape {
    override def show(): Unit = {
      print("红色")
      warp.show()
    }

  }

  class Blue(override val warp: BasicShape) extends ColorfulShape {
    override def show(): Unit = {
      print("蓝色")
      warp.show()
    }

  }

  class Square(override val warp: BasicShape) extends ColorfulShape {
    override def show(): Unit = {
      print("方")
      warp.show()
    }

  }

  def main(args: Array[String]): Unit = {

    Seq(new Zhu(), new Shape())
      .flatMap(x => {
        Seq(new Circle(x), new Square(x))
      }).flatMap(x => {
      Seq(new Blue(x), new Red(x))
    }).foreach(_.show())

  }
}


object DecoratorPatternExample {

  trait Shape {
    def draw(): Unit
  }

  class Circle extends Shape {
    override def draw(): Unit = print("圆形")
  }

  class Square extends Shape {
    override def draw(): Unit = print("方形")
  }

  trait ColorDecorator extends Shape {
    protected val decoratedShape: Shape

    override def draw(): Unit = decoratedShape.draw()
  }

  class Red(override val decoratedShape: Shape) extends ColorDecorator {

    override def draw(): Unit = {
      super.draw()
      print("红色")
    }
  }

  class Blue(override val decoratedShape: Shape) extends ColorDecorator {
    override def draw(): Unit = {
      super.draw()
      print("蓝色")
    }
  }

  def main(args: Array[String]): Unit = {
    val circle = new Circle()
    val redCircle = new Red(circle)
    redCircle.draw()

    val square = new Square()
    val blueSquare = new Blue(square)
    blueSquare.draw()
  }
}

object CubeExample {
  def main(args: Array[String]): Unit = {
    val size = 2

    // 打印立方体的顶部
    for (i <- 0 until size) print(" ")
    println("+--------+")

    // 打印立方体的侧面
    for (i <- 0 until size) {
      for (j <- 0 until size - i - 1) print(" ")
      print("/")
      for (j <- 0 until size) print(" ")
      println(" /|")
    }

    // 打印立方体的底部
    print("+")
    for (i <- 0 until size) print("-")
    println("-+ |")
    for (i <- 0 until size) {
      print("|")
      for (j <- 0 until size) print(" ")
      println(" | |")
    }
    print("|")
    for (i <- 0 until size) print(" ")
    println(" | +")
    for (i <- 0 until size) {
      print("|")
      for (j <- 0 until size) print(" ")
      println(" |/ ")
    }
    print("+")
    for (i <- 0 until size) print("-")
    println("-+")
  }
}
