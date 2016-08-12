package com.asyaminor.challenge

/**
  * Created by eunlu on 12/08/2016.
  */
abstract class Paint
case class EmptyCan() extends Paint
case class ProducedPaint(typePaint: PaintType)
abstract class PaintType
case class Matte() extends PaintType
case class Gloss() extends PaintType
