package org.eterk.klearn.untyped

object Transient {

  import java.io._

  private case class PersonA(name: String, @transient age: Int)

  private case class PersonB(name: String, age: Int)

  private def writeAndRead[T](x: T): T = {
    val path = "person.obj"
    new File(path).delete()
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(x)
    oos.close()

    val ois = new ObjectInputStream(new FileInputStream(path))
    val res = ois.readObject().asInstanceOf[T]
    ois.close()
    new File(path).delete()
    res
  }

  def main(args: Array[String]): Unit = {
    println(writeAndRead(PersonA("Alice", 25))) //Person(Alice,0)
    println(writeAndRead(PersonB("Alice", 25))) //Person(Alice,25)
  }

}


