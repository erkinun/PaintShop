package com.asyaminor.challenge

import org.scalatest.FunSuite
import com.asyaminor.challenge.PaintShop.BucketPrint

/**
  * Created by eunlu on 14/08/2016.
  */
class PaintShopTest extends FunSuite {
  val paintShop = PaintShop

  def getBucketAndCustomers(fileName: String): (List[String], List[ProducedPaint]) = {
    val (colorCount, customerList) = paintShop.parseInputFile(fileName)
    val bucket = List.fill(colorCount)(ProducedPaint(Gloss()))
    (customerList, bucket)
  }

  test("parse the first input file ex1, color count must be 5") {
    val (colorCount, _) = paintShop.parseInputFile("inputs/ex1.txt")

    assert(colorCount == 5)
  }

  test("parse the first input file ex1, customer list must match") {
    val (_, customerStrList) = paintShop.parseInputFile("inputs/ex1.txt")

    val expectedList = List("1 M 3 G 5 G", "2 G 3 M 4 G", "5 M")

    assert(expectedList.equals(customerStrList))
  }

  test("ex1 must return the result of G G G G M") {
    val (customerList: List[String], bucket: List[ProducedPaint]) = getBucketAndCustomers("inputs/ex1.txt")

    val bucketResult = paintShop.fixThePaintBucket(bucket, customerList)

    val expectedOutput = "G G G G M"

    println(bucketResult.printBucket)
    assert(expectedOutput.equals(bucketResult.printBucket))
  }

  test("ex2 must return the result of M M") {
    val (customerList: List[String], bucket: List[ProducedPaint]) = getBucketAndCustomers("inputs/ex2.txt")

    val bucketResult = paintShop.fixThePaintBucket(bucket, customerList)

    val expectedOutput = "M M"

    println(bucketResult.printBucket)
    assert(expectedOutput.equals(bucketResult.printBucket))
  }

  test("ex3 must return the result of G M G M G") {
    val (customerList: List[String], bucket: List[ProducedPaint]) = getBucketAndCustomers("inputs/ex3.txt")

    val bucketResult = paintShop.fixThePaintBucket(bucket, customerList)

    val expectedOutput = "G M G M G"

    println(bucketResult.printBucket)
    assert(expectedOutput.equals(bucketResult.printBucket))
  }

  test("nosol must throw the result of No solution exists") {
    val (customerList: List[String], bucket: List[ProducedPaint]) = getBucketAndCustomers("inputs/nosol.txt")


    val expectedOutput = "No solution exists"

    val thrown = intercept[NoSolutionCanBeFound] {
      paintShop.fixThePaintBucket(bucket, customerList)
    }

    assert(expectedOutput.equals(thrown.toString))
  }
}
