package com.asyaminor.challenge

import scala.io.Source

/**
  * Created by eunlu on 12/08/2016.
  */
object PaintShop {
  def main(args: Array[String]) {
    println("Hello PaintShop!")

    if (args.length == 0) {
      println("Filename must be supplied from the command line")
    }
    else {
      val fileName = args(0)
      println(s"file name provided: $fileName")

      for (line <- Source.fromFile(fileName).getLines()) {

      }
    }
  }
}
