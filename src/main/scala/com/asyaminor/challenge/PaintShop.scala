package com.asyaminor.challenge

import scala.io.Source

/**
  * Created by eunlu on 12/08/2016.
  */
object PaintShop {

  type Likes = Int
  type Preference = (Int, Char)

  private def getPaintType(colorType: Char): PaintType = colorType match {
    case 'M' => Matte()
    case 'G' => Gloss()
    case _ => throw new IllegalArgumentException
  }

//  def changeBucket(bucket: List[Paint], tuples: List[(Int, Char)], likes: List[Likes]): List[Paint] = tuples match {
//    case Nil => bucket
//    case (colorNum, typeOfPaint) :: rest =>
//      //get the color object for this
//      val paintType = getPaintType(typeOfPaint)
//
//      //match it with the current color in the bucket
//      val colorIndex = colorNum - 1
//      val color = bucket(colorIndex)
//
//      color match {
//        case EmptyCan() =>
//          changeBucket(bucket.updated(colorIndex, ProducedPaint(paintType)), rest)
//        case ProducedPaint(anyColor) => if (anyColor.equals(paintType)) changeBucket(bucket, rest)
//        else {
//          println(s"Bucket at index: $colorNum has already paint type of: $anyColor but customer wanted: $paintType")
//          throw new NoSolutionCanBeFound()
//        }
//      }
//    //if its already matte or gloss and is not equal, fail
//    //or change the color and continue
//  }

  def fixThePaintBucket(bucket: List[Paint], customerList: List[String]): List[Paint] = {

    def turnIntoTupleList(input: List[String]): List[Preference] = {

      def turnInner(input: List[String], tuples: List[Preference]): List[(Int, Char)] = {
        input match {
          case i :: c :: rest =>
            val number = i.toInt
            val typeOfColor = c(0)
            turnInner(rest, (number, typeOfColor) :: tuples)
          case _ => tuples
        }
      }

      turnInner(input, List()).reverse
    }

    customerList match {
      case Nil => bucket
      case cust :: rest =>
        val tuples = turnIntoTupleList(cust.split(" ").toList)

        //val newBucket = changeBucket(bucket, tuples, List())

        fixThePaintBucket(bucket, rest)
    }
  }

//  def main(args: Array[String]) {
//    println("Hello PaintShop!")
//
//    if (args.length == 0) {
//      println("Filename must be supplied from the command line")
//    }
//    else {
//      val fileName = args(0)
//      println(s"file name provided: $fileName")
//
//      val inputLines = Source.fromFile(fileName).getLines().toList
//
//      require(inputLines.nonEmpty)
//
//      val colorLength = inputLines(0).toInt
//      val customerList = inputLines.tail
//
//      val bucket = List.fill(colorLength)(ProducedPaint(Gloss()))
//
//      val newBucket = fixThePaintBucket(bucket, customerList)
//
//      //replace it with toString
//      newBucket foreach(paint => println(paint))
//    }
//  }
}
