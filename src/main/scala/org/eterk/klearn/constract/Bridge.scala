package org.eterk.klearn.constract

// 颜色接口
trait Color {
  def applyColor(): Unit
}

// 红色实现类
class Red extends Color {
  override def applyColor(): Unit = println("红色")
}

// 蓝色实现类
class Blue extends Color {
  override def applyColor(): Unit = println("蓝色")
}

// 形状抽象类
abstract class Shape(color: Color) {
  def draw(): Unit
}

// 圆形实现类
class Circle(color: Color) extends Shape(color) {
  override def draw(): Unit = {
    print("圆形")
    color.applyColor()
  }
}

// 方形实现类
class Square(color: Color) extends Shape(color) {
  override def draw(): Unit = {
    print("方形")
    color.applyColor()
  }
}

object BridgePatternExample {
  def main(args: Array[String]): Unit = {
    val redCircle = new Circle(new Red)
    redCircle.draw()

    val blueSquare = new Square(new Blue)
    blueSquare.draw()
  }
}
