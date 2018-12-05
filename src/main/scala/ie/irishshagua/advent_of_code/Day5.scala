package ie.irishshagua.advent_of_code

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  require({
    val input = "dabAcCaCBAcCcaDA"
    val reduction = reduce(input)
    val optimisedReduction = unitIsolationReduction(input)

    println(s"Reduction: $reduction")
    println(s"Optimised Reduction: $optimisedReduction")

    reduction == "dabCBAcaDA" && reduction.length == 10 && optimisedReduction == ('c', "daDA") && optimisedReduction._2.length == 4
  })

  val input = Source.fromResource("day5/input").getLines().toList.head
  println(s"Day 5 Part 1 Result: ${reduce(input).length}")
  println(s"Day 5 Part 2 Result: ${unitIsolationReduction(input)._2.length}")

  @tailrec
  def reduce(input: String): String = {
    val reduced = input.foldLeft((None: Option[Char], new StringBuilder)) {
      case ((None, sb), char) => (Some(char), sb.append(char))
      case ((Some(prevChar), sb), char) =>
        if (Math.abs(prevChar.toInt - char.toInt) == 32) (None, if (sb.nonEmpty) sb.deleteCharAt(sb.length - 1) else sb)
        else (Some(char), sb.append(char))
    }._2.toString()

    if (reduced == input) reduced
    else reduce(reduced)
  }

  def unitIsolationReduction(input: String): (Char, String) = {
    val allReductions = for {
      unit <- 'a' to 'z'
      isolatedPolymer = input.replaceAll(unit.toString, "").replaceAll((unit & 0x5f).asInstanceOf[Char].toString, "")
    } yield (unit, reduce(isolatedPolymer))

    allReductions.minBy {_._2.length}
  }
}
