package ie.irishshagua.advent_of_code

import scala.annotation.tailrec
import scala.io.Source

case class CurrentAdjustmentState(currentState: Int = 0, firstDuplicatedState: Option[Int] = None, previousStates: Set[Int] = Set(0))

class WrapAroundIterator[T](ls: List[T]) extends Iterator[T] {

  private var internalIter = ls.iterator

  override def hasNext: Boolean =
    internalIter.hasNext || {
      internalIter = ls.iterator
      true
    }

  override def next(): T =
    internalIter.next()
}

object Day1 extends App {

  require(applyAllAdjustments(List("+1", "+1", "+1").map(decode)).currentState == 3)
  require(applyAllAdjustments(List("+1", "+1", "-2").map(decode)).currentState == 0)
  require(applyAllAdjustments(List("-1", "-2", "-3").map(decode)).currentState == -6)


  require(findFirstDuplicate(new WrapAroundIterator(List("+1", "-1").map(decode))).firstDuplicatedState.contains(0))
  require(findFirstDuplicate(new WrapAroundIterator(List("+3", "+3", "+4", "-2", "-4").map(decode))).firstDuplicatedState.contains(10))
  require(findFirstDuplicate(new WrapAroundIterator(List("-6", "+3", "+8", "+5", "-6").map(decode))).firstDuplicatedState.contains(5))
  require(findFirstDuplicate(new WrapAroundIterator(List("+7", "+7", "-2", "-7", "-4").map(decode))).firstDuplicatedState.contains(14))

  val input = Source.fromResource("day1/input").getLines().toList.map(decode)
  val result1 = applyAllAdjustments(input)
  val result2 = findFirstDuplicate(new WrapAroundIterator(input))
  println(s"Day1 Part 1 Output: ${result1.currentState}")
  println(s"Day1 Part 2 Output: ${result2.firstDuplicatedState}")

  Stream.continually(input)

  @tailrec
  def applyAllAdjustments(adjustments: List[Int], currentState: CurrentAdjustmentState = CurrentAdjustmentState()): CurrentAdjustmentState = adjustments match {
    case Nil => currentState
    case hd :: tail => applyAllAdjustments(tail, currentState.copy(currentState = adjust(currentState.currentState, hd)))
  }

  @tailrec
  def findFirstDuplicate(adjustments: WrapAroundIterator[Int], currentState: CurrentAdjustmentState = CurrentAdjustmentState()): CurrentAdjustmentState = currentState.firstDuplicatedState match {
    case Some(_) => currentState
    case None => if (adjustments.hasNext) {
      val nextAdjustment = adjustments.next()
      val adjustedState = adjust(currentState.currentState, nextAdjustment)
      val dupeState = Some(adjustedState).filter(currentState.previousStates.contains)
      findFirstDuplicate(adjustments, currentState.copy(currentState = adjustedState, firstDuplicatedState = dupeState, previousStates = currentState.previousStates + adjustedState))
    } else {
      currentState
    }
  }

  def decode(value: String): Int =
    value.toInt

  def adjust(currentState: Int, adjustment: Int): Int =
    currentState + adjustment
}
