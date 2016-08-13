package com.asyaminor.challenge

/**
  * Created by eunlu on 12/08/2016.
  */
abstract class Paint
case class EmptyCan() extends Paint
case class ProducedPaint(typePaint: PaintType) extends Paint
abstract class PaintType
case class Matte() extends PaintType {
  override def toString = "M"
}
case class Gloss() extends PaintType {
  override def toString = "G"
}
