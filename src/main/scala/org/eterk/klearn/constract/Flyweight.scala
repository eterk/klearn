package org.eterk.klearn.constract

object Flyweight {

  // 享元接口
  trait Shape {
    def draw(): Unit
  }

  // 具体享元类
  class Circle(var x: Int, var y: Int, val radius: Int, val color: String) extends Shape {
    override def draw(): Unit =
      println(s"Circle: Draw() [Color : $color, x : $x, y : $y, radius : $radius]")
  }

  // 享元工厂类
  object ShapeFactory {
    private val circleMap = scala.collection.mutable.Map[String, Shape]()

    def getCircle(color: String): Shape = {
      var circle = circleMap.getOrElseUpdate(color, new Circle(0, 0, 100, color))
      circle
    }
  }

  // 客户端代码
  private val colors = Array("Red", "Green", "Blue", "White", "Black")

  def main(args: Array[String]): Unit = {
    for (i <- 0 until 20) {
      val circle = ShapeFactory.getCircle(getRandomColor)
      circle.asInstanceOf[Circle].x = getRandomX
      circle.asInstanceOf[Circle].y = getRandomY
      circle.draw()
    }
  }

  private def getRandomColor: String = colors((Math.random * colors.length).toInt)

  private def getRandomX: Int = (Math.random * 100).toInt

  private def getRandomY: Int = (Math.random * 100).toInt


}
