package org.eterk.klearn.constract

trait Factory {
  def create: Product
}

class FactoryA extends Factory {
  override def create: Product = new ProductA
}

class FactoryB extends Factory {
  override def create: Product = new ProductB
}

trait Product {
  def name: String
}

class ProductA extends Product {
  override def name: String = "A"
}

class ProductB extends Product {
  override def name: String = "B"
}

