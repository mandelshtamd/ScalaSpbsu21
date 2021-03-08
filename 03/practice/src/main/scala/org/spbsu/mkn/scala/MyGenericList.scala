package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.annotation.tailrec

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[T1](f: T => T1): MyGenericList[T1]
  def ::[T1 >: T] (elem: T1): MyGenericList[T1]
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = seq match {
    case Nil => MyNil
    case _ => seq.head :: (this fromSeq seq.tail)
  }

  def sum[T : Numeric](list: MyGenericList[T]): T = list match {
    case MyNil => undef
    case _ => foldLeft(implicitly[Numeric[T]].zero)((el: T, headSum: T) => implicitly[Numeric[T]].plus(el, headSum))(list)
  }

  def size[T](list: MyGenericList[T]): Int = foldLeft(0)((_: T, headSize: Int) => headSize + 1)(list)

  @tailrec
  def foldLeft[T1, T](ini: T1)(f: (T, T1) => T1)(list: MyGenericList[T]): T1 = list match {
    case MyNil => ini
    case MyCons(h, t) => foldLeft(f(h, ini))(f)(t)
  }
}

case object MyNil extends MyGenericList[Nothing] {
  override def ::[T >: Nothing](elem: T): MyGenericList[T] = MyCons(elem, MyNil)
  override def head: Nothing = undef
  override def tail: MyGenericList[Nothing] = undef

  override def drop(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyNil
    case _ => undef
  }

  override def take(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyNil
    case _ => undef
  }

  override def map[T](f: Nothing => T): MyGenericList[T] = MyNil
}

case class MyCons[+T](head: T, tail: MyGenericList[T]) extends MyGenericList[T] {
  override def ::[T1 >: T](elem: T1): MyGenericList[T1] = MyCons(elem, head :: tail)

  override def drop(n: Int): MyGenericList[T] = n match {
    case 0 => this
    case n if n > 0 => tail drop(n - 1)
    case _ => undef
  }

  override def take(n: Int): MyGenericList[T] = n match {
    case 0 => MyNil
    case n if n > 0 => head :: (tail take(n - 1))
    case _ => undef
  }

  override def map[T1](f: T => T1): MyGenericList[T1] = f(head) :: (tail map f)
}