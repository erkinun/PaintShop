package com.asyaminor.challenge

import scala.io.Source

/**
  * Created by eunlu on 12/08/2016.
  */
object PaintShop {

  def fixThePaintBucket(bucket: List[Paint], customerList: List[String]): Unit = ???

  def main(args: Array[String]) {
    println("Hello PaintShop!")

    if (args.length == 0) {
      println("Filename must be supplied from the command line")
    }
    else {
      val fileName = args(0)
      println(s"file name provided: $fileName")

      val inputLines = Source.fromFile(fileName).getLines().toList

      require(inputLines.nonEmpty)

      val colorLength = inputLines(0).toInt
      val customerList = inputLines.tail

      val bucket = List.fill(colorLength)(EmptyCan())

      customerList.foreach(customer => println(customer))

      fixThePaintBucket(bucket, customerList)
    }
  }
}
