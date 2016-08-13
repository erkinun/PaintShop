package com.asyaminor.challenge

import scala.io.Source

/**
  * Created by eunlu on 13/08/2016.
  */
object PaintShopV2 {

  type Bucket = List[ProducedPaint]
  type Likes = Int
  type Preference = (Int, Char)
  type Customer = List[Preference]
  type Choices = Map[String, List[Int]]

  private def getPaintType(colorType: Char): PaintType = colorType match {
    case 'M' => Matte()
    case 'G' => Gloss()
    case _ => throw new IllegalArgumentException
  }



  def fixThePaintBucket(bucket: Bucket, customerList: List[String]): Bucket = {
    def turnIntoTupleList(input: List[String]): Customer = {

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

    def increaseLikes(likes: List[Likes], index: Int): List[Likes] = {
      val current = likes(index)
      likes.updated(index, current + 1)
    }
    def decreaseLikes(likes: List[Likes], index: Int): List[Likes] = {
      val current = likes(index)

      if (current < 1) {
        throw new NoSolutionCanBeFound()
      }

      likes.updated(index, current - 1)
    }

    def customersOfPref(color: Int, paintType: PaintType, choices: Choices): Option[List[Int]] = {
      val key = color + paintType.toString
      Some(choices(key))
    }

    def makePeopleUnhappy(color: Int, paintType: PaintType, choices: Choices, likes: List[Likes]): List[Likes] = {
      customersOfPref(color, paintType, choices) match {
        case Some(people) =>
          people.foldLeft(likes)((likeS, person) => decreaseLikes(likeS, person))
        case None => likes
      }
    }

    def recordCustomerPref(choices: Choices, customerIndex: Int, paintType: PaintType, color: Int): Choices = {
      val key  = color + paintType.toString
      val pref = choices.get(key)

      pref match {
        case Some(preference) => choices.updated(key, customerIndex :: preference)
        case None => choices + (key -> List(customerIndex))
      }
    }

    def solveForCustomer(cust: Customer, bucket: Bucket,
                         choices: Choices,
                         likes: List[Likes], index: Likes): (Bucket, Choices, List[Likes]) = {

      cust match {
        case Nil => (bucket, choices, likes)
        case pref :: otherPrefs =>
          //if color matches
          val color = pref._1
          val paintType = getPaintType(pref._2)

          if (bucket(color).typePaint == paintType) {
            solveForCustomer(otherPrefs, bucket,
              recordCustomerPref(choices, index, paintType, color),
              increaseLikes(likes, index), index)
          }
          else {
            val likesNew = makePeopleUnhappy(color, bucket(color).typePaint, choices, likes)
            solveForCustomer(otherPrefs, bucket, recordCustomerPref(choices, index, paintType, color),
              increaseLikes(likesNew, index), index)
          }
      }
    }

    val preferenceList = customerList map
      {customerString => turnIntoTupleList(customerString.split(" ").toList)}

    val sortedPrefs = preferenceList sortBy(pref => pref.size)
    //init likes list
    val likesByCustomers = List.fill(customerList.size)(0)
    //choices by customers
    val choices: Choices = Map()

    def tryToSolve(bucket: Bucket, choices: Choices, likes: List[Likes], customerList: List[Customer],
                   index: Int): Bucket = {
      customerList match {
        case Nil => bucket //we can handle the reductions here!!! //TODO also check if there
        case cust :: rest =>
          val (bucketN, choicesN, likesN) = solveForCustomer(cust, bucket, choices, likes, index)
          tryToSolve(bucketN, choicesN, likesN, rest, index + 1)
      }
    }


    //foreach customer
    customerList match {
      case Nil => bucket
      case cust :: rest => tryToSolve(bucket, choices, likesByCustomers, sortedPrefs, 0)
    }

    //handle reductions and check for finished solution
    throw new NotImplementedError("reductions have to be handled")
  }

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

      val bucket = List.fill(colorLength)(ProducedPaint(Gloss()))

      val newBucket = fixThePaintBucket(bucket, customerList)

      //replace it with toString
      newBucket foreach(paint => println(paint))
    }
  }
}
