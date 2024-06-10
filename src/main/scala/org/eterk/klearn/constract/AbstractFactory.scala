package org.eterk.klearn.constract

trait AbstractFactory {
  def createCPU: CPU

  def createStorage: Storage

  def createScreen: Screen
}

class PhoneFactory extends AbstractFactory {
  override def createCPU: CPU = new PhoneCpu

  override def createStorage: Storage = new PhoneMemory

  override def createScreen: Screen = new SmallScreen
}

class ComputerFactory extends AbstractFactory {
  override def createCPU: CPU = new DesktopCpu

  override def createStorage: Storage = new ComputerMemory

  override def createScreen: Screen = new BigScreen
}

trait CPU

class PhoneCpu extends CPU

class DesktopCpu extends CPU

trait Storage

class PhoneMemory extends Storage

class ComputerMemory extends Storage


trait Screen

class BigScreen extends Screen

class SmallScreen extends Screen

