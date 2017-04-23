package chapter2

import scala.annotation.tailrec

object IsSorted extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def isSorted0(idx: Int): Boolean = {
      if (idx == as.length - 1) true
      else {
        if (ordered(as(idx), as(idx + 1))) isSorted0(idx + 1)
        else false
      }
    }

    if (as.length == 0) true
    else isSorted0(0)
  }

  val arr1 = Array(1, 3, 2, 5)
  val arr2 = Array(3)
  val arr3 = Array(3, 1, 4)

  def ltInts(x: Int, y: Int): Boolean = x <= y

  println("arr1: " + isSorted(arr1, ltInts))
  println("arr2: " + isSorted(arr2, ltInts))
  println("arr3: " + isSorted(arr3, ltInts))
}
