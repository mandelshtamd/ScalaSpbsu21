package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = seq match {
    case Nil => IntNil
    case _ => seq.head :: (this fromSeq seq.tail)
  }

  def sum(intList: IntList): Int = {
    intList match {
      case IntNil => undef
      case _ => foldLeft(intList, 0) {_ + _}
    }
  }

  def size(intList: IntList): Int = {
    intList match {
      case IntNil => 0
      case IntCons(_, list) => 1 + size(list)
    }
  }

  // extra task: implement sum using foldLeft
  @tailrec
  def foldLeft[T](intList: IntList, initValue: T)(f: (T, Int) => T): T = {
    intList match {
      case IntNil => initValue
      case IntCons(x, list) => foldLeft(list, f(initValue, x))(f)
    }
  }
}

case object IntNil extends IntList {
  override def ::(elem: Int): IntList = IntCons(elem, IntNil)

  override def head: Int = undef

  override def tail: IntList = IntNil

  override def drop(n: Int): IntList = n match {
    case 0 => IntNil
    case _ => undef
  }

  override def take(n: Int): IntList = n match {
    case 0 => IntNil
    case _ => undef
  }

  override def map(f: Int => Int): IntList = IntNil
}

case class IntCons(head: Int, tail: IntList) extends IntList {
  override def ::(elem: Int): IntList = IntCons(elem, head :: tail)

  override def drop(n: Int): IntList = n match {
    case 0 => this
    case _ => tail drop(n - 1)
  }

  override def take(n: Int): IntList = n match {
    case 0 => IntNil
    case _ => head :: (tail take(n - 1))
  }

  override def map(f: Int => Int): IntList = f(head) :: (tail map f)
}