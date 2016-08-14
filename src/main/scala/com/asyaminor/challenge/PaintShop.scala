package com.asyaminor.challenge

import java.io.File

import scala.io.Source

/**
  * Created by eunlu on 13/08/2016.
  * main algoritm :
  * read each customer
  * turn them into a tuple list
  * sort them
  * create likes list to store status of each customers happiness
  * create choice map to store which customer prefers which color and type
  * start with the customer with fewest number of preferences
  * when found a color on the bucket that matches with a customer
  * increase its happiness, store the preference
  * if there is a changing color, make other people unhappy
  * if you can't make people unhappy, that means there is no solution
  * when all customers are done try to reduce the price on color bucket
  * check each matte paint and try to turn them over to gloss
  * when the first iteration is done, check if the bucket has changed
  * if bucket changed, then iterate until bucket finishes changing
  * that's the perfect bucket
  *
  * Note: simple functions are not documented
  */
object PaintShop {

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

  /**
    * Helper implicit to add a printing method to type Bucket
   */
  implicit class BucketPrint(bucket: Bucket) {
    def printBucket = bucket.foldLeft("")((str, paint) => str + " " + paint.typePaint.toString).trim
  }

  def fixThePaintBucket(bucket: Bucket, customerList: List[String]): Bucket = {

    def turnIntoCustomer(input: List[String]): Customer = {

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
    /**
      *
      * @param likes
      * @param index
      * @return updated list of likes
      *         This method also has the ability to finish the simulation
      *         which may be a bad idea that can be refactored later on
      */
    def decreaseLikes(likes: List[Likes], index: Int): List[Likes] = {
      val current = likes(index)

      if (current == 1) {
        throw new NoSolutionCanBeFound()
      }

      likes.updated(index, current - 1)
    }
    /**
      *
      * @param color
      * @param paintType
      * @param choices
      * @return an option of people who might prefer the particular color with paint type
      */
    def customersOfPref(color: Int, paintType: PaintType, choices: Choices): Option[List[Int]] = {
      val key = color + paintType.toString
      //println(s"key: $key and choices: " + choices)
      choices.get(key)
    }
    /**
      *
      * @param color
      * @param paintType
      * @param choices
      * @param likes
      * @return updated list of likes using fold for iteration
      *         if we have people that like the color and painttype, we will make them happier
      */
    def makePeopleHappy(color: Int, paintType: PaintType, choices: Choices, likes: List[Likes]): List[Likes] = {

      //println(s"making people happy with paint $paintType")

      customersOfPref(color + 1, paintType, choices) match {
        //customersOfPref(color, Matte(), choices) match {
        case Some(people) =>
          //println(s"found happy people: $people")
          people.foldLeft(likes)((likeS, person) => increaseLikes(likeS, person))
        case None => likes
      }
    }

    def makePeopleUnhappy(color: Int, paintType: PaintType, choices: Choices, likes: List[Likes]): List[Likes] = {

      //println(s"making people unhappy with paint $paintType")

      customersOfPref(color + 1, paintType, choices) match {
      //customersOfPref(color, Matte(), choices) match {
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

    /**
      *
      * @param index
      * @param choices
      * @param likes
      * @return true/false
      *         we check people with a given color pref,
      *         if all the people who prefers the color combination
      *         has more than 1 like, we return true
      *         else false
      */
    def canWeMakeUnhappy(index: Likes, choices: Choices, likes: List[Likes]): Boolean = {
      // we are only turning to Gloss for getting cheaper

      //println(s"looking for index: ${index + 1} with Matte choices")

      customersOfPref(index + 1, Matte(), choices) match {
        case Some(people) =>
          //println(s"people for matte on index ${index + 1} count is: " + people.size)
          val filteredPeople = people.count{person => likes(person) > 1}
          filteredPeople == people.size
        case None => true
      }
    }

    /**
      *
      * @param bucket
      * @param choices
      * @param likes
      * @param index
      * @return update bucket
      *         this method is where we try to reduce the price of bucket
      *         we take a look at each Matte color, and try to convert it ot Gloss color
      *         if the selector of Matte color has likes more than 1, we can convert it
      *         the inner method is called as long as the old bucket and the reducted bucket
      *         are different which means there might be still a chance to reduce its price
      */
    def makeItCheaper(bucket: Bucket,
                      choices: Choices, likes: List[Likes], index: Int): Bucket = {

      def makeCheapInner(bucket: Bucket, newBucket: Bucket,
                         choices: Choices, likes: List[Likes], index: Int): (Bucket, List[Likes]) = bucket match {
        case Nil => (newBucket, likes)
        case paint :: rest =>
          if (paint.typePaint == Matte() && canWeMakeUnhappy(index, choices, likes)) {
            val currentLikes = makePeopleUnhappy(index, Matte(), choices, likes)
            val updatedLikes = makePeopleHappy(index, Gloss(), choices, currentLikes)
            makeCheapInner(rest, newBucket.updated(index, ProducedPaint(Gloss())), choices, updatedLikes, index + 1)
          }
          else {
            makeCheapInner(rest, newBucket, choices, likes, index + 1)
          }
      }

      val (updatedBucket, updatedLikes) = makeCheapInner(bucket, newBucket = bucket, choices, likes, index)

      if (updatedBucket.equals(bucket)) {
        updatedBucket
      }
      else {
        makeItCheaper(updatedBucket, choices, updatedLikes, index)
      }

    }

    def solveForCustomer(cust: Customer, bucket: Bucket,
                         choices: Choices,
                         likes: List[Likes], index: Likes): (Bucket, Choices, List[Likes]) = {

      def getPaintFromBucket(bucket: Bucket, color: Int) = {
        bucket(color - 1).typePaint
      }

      //println(s"trying to solve for customer with index: $index, customer has $prefCount preferences left")
      //println("current likes: " + likes)

      cust match {
        case Nil => (bucket, choices, likes)
        case pref :: otherPrefs =>
          //if color matches
          val color = pref._1
          val paintType = getPaintType(pref._2)

          if (getPaintFromBucket(bucket, color) == paintType || paintType == Gloss()) {
            if (getPaintFromBucket(bucket, color) == paintType) {
              // we only increase happiness if there is a match
              solveForCustomer(otherPrefs, bucket,
                recordCustomerPref(choices, index, paintType, color),
                increaseLikes(likes, index), index)
            }
            else {
              // if there is already a matte color and customer wanted gloss
              // we can skip that for now, recording the selection though
              solveForCustomer(otherPrefs, bucket,
                recordCustomerPref(choices, index, paintType, color),
                likes, index)
            }

          }
          else {
            // this means we have matte color, we have to switch to that
            val bucketNew = bucket.updated(color - 1, ProducedPaint(paintType))

            val likesNew = makePeopleUnhappy(color - 1, getPaintFromBucket(bucket, color), choices, likes)
            solveForCustomer(otherPrefs, bucketNew , recordCustomerPref(choices, index, paintType, color),
              increaseLikes(likesNew, index), index)
          }
      }
    }

    val preferenceList = customerList map
      {customerString => turnIntoCustomer(customerString.split(" ").toList)}

    val sortedPrefs = preferenceList sortBy(pref => pref.size)
    //init likes list
    val likesByCustomers = List.fill(customerList.size)(0)
    //choices by customers
    val choices: Choices = Map()

    def tryToSolve(bucket: Bucket, choices: Choices, likes: List[Likes], customerList: List[Customer],
                   index: Int): Bucket = {
      customerList match {
        case Nil =>
          val bucketString = bucket.printBucket
//          println(s"before reduction: $bucketString")
          val revisedBucket = makeItCheaper(bucket, choices, likes, index = 0)
          //bucket //we can handle the reductions here!!! //TODO also check if there
          revisedBucket
        case cust :: rest =>
          val (bucketN, choicesN, likesN) = solveForCustomer(cust, bucket, choices, likes, index)
//          println("current bucket: " + bucketN.printBucket)
          tryToSolve(bucketN, choicesN, likesN, rest, index + 1)
      }
    }


    //foreach customer
    customerList match {
      case Nil => bucket
      case cust :: rest => tryToSolve(bucket, choices, likesByCustomers, sortedPrefs, 0)
    }
  }

  def main(args: Array[String]) {

    //TODO think about corner cases

    if (args.length == 0) {
      println("Filename must be supplied from the command line")
    }
    else {
      val fileName = args(0)

      val (colorLength: Int, customerList: List[String]) = parseInputFile(fileName)

      val bucket = List.fill(colorLength)(ProducedPaint(Gloss()))

      try {
        val newBucket = fixThePaintBucket(bucket, customerList)

        println(newBucket.printBucket)
      }
      catch {
        case nsfe: NoSolutionCanBeFound => println("No solution exists")
        case t: Throwable => println("unknown error" + t.getMessage )
      }

    }
  }

  def parseInputFile(fileName: String): (Int, List[String]) = {

    require(new File(fileName).exists())

    val inputLines = Source.fromFile(fileName).getLines().toList

    require(inputLines.nonEmpty)

    val colorLength = inputLines(0).toInt
    val customerList = inputLines.tail
    (colorLength, customerList)
  }
}
