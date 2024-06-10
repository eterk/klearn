package org.eterk.klearn.constract

// 组件抽象类
abstract class Component(name: String) {
  def add(component: Component): Unit
  def remove(component: Component): Unit
  def display(depth: Int): Unit
}

// 叶子节点实现类
class Leaf(name: String) extends Component(name) {
  override def add(component: Component): Unit =
    println("Cannot add to a leaf")

  override def remove(component: Component): Unit =
    println("Cannot remove from a leaf")

  override def display(depth: Int): Unit =
    println("-" * depth + name)
}

// 组合节点实现类
class Composite(name: String) extends Component(name) {
  private val children = scala.collection.mutable.Buffer[Component]()

  override def add(component: Component): Unit =
    children += component

  override def remove(component: Component): Unit =
    children -= component

  override def display(depth: Int): Unit = {
    println("-" * depth + name)
    children.foreach(_.display(depth + 2))
  }
}

object CompositePatternExample {
  def main(args: Array[String]): Unit = {
    val root = new Composite("root")
    root.add(new Leaf("Leaf A"))
    root.add(new Leaf("Leaf B"))

    val comp = new Composite("Composite X")
    comp.add(new Leaf("Leaf XA"))
    comp.add(new Leaf("Leaf XB"))

    root.add(comp)

    val comp2 = new Composite("Composite XY")
    comp2.add(new Leaf("Leaf XYA"))
    comp2.add(new Leaf("Leaf XYB"))

    comp.add(comp2)

    root.add(new Leaf("Leaf C"))

    val leaf = new Leaf("Leaf D")
    root.add(leaf)
    root.remove(leaf)

    root.display(1)
  }
}

