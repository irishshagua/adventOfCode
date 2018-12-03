package ie.irishshagua.advent_of_code

import scala.io.Source

object Day1 extends App {

  require(apply(List("+1", "+1", "+1")) == 3)
  require(apply(List("+1", "+1", "-2")) == 0)
  require(apply(List("-1", "-2", "-3")) == -6)

  val input = Source.fromResource("day1/input").getLines().toList
  val result = apply(input)
  println(s"Day1 Output: $result")

  def apply(adjustments: List[String], initialState: Int = 0): Int = adjustments match {
    case Nil => initialState
    case hd :: tail => apply(tail, adjust(initialState, decode(hd)))
  }

  def decode(value: String): Int =
    value.toInt

  def adjust(currentState: Int, adjustment: Int): Int =
    currentState + adjustment
}
