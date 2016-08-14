package com.asyaminor.challenge

/**
  * Created by eunlu on 12/08/2016.
  */
case class NoSolutionCanBeFound() extends Exception {
  override def toString = "No solution exists"
}
