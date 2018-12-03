package ie.irishshagua.advent_of_code

import scala.io.Source

case class Dimensions(width: Int, height: Int)
case class Claim(id: Int, x: Int, y: Int, dimensions: Dimensions)

object Day3 extends App {

  val ValidClaimPattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r.pattern

  val input = Source.fromResource("day3/input").getLines().flatMap(toClaim)
  input.foreach(println)

  def toClaim(detail: String): Option[Claim] = {
    val matcher = ValidClaimPattern.matcher(detail)
    if (matcher.matches()) {
      Some(Claim(
        id = matcher.group(1).toInt,
        x = matcher.group(2).toInt,
        y = matcher.group(3).toInt,
        dimensions = Dimensions(
          width = matcher.group(4).toInt,
          height = matcher.group(5).toInt
        )
      ))
    } else None
  }
}
