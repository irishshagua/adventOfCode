package ie.irishshagua.advent_of_code

import scala.io.Source

case class Dimensions(width: Int, height: Int)
case class Claim(id: Int, x: Int, y: Int, dimensions: Dimensions)

object Day3 extends App {

  val ValidClaimPattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r.pattern

  require(multiRequestedArea(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").flatMap(toClaim)) == 4)
  require(uniqueClaim(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").flatMap(toClaim)).map(_.id).contains(3))

  val input = Source.fromResource("day3/input").getLines().flatMap(toClaim).toList
  println(s"Total Duplicated Requested Area: ${multiRequestedArea(input)}")
  println(s"Unique Claim: ${uniqueClaim(input)}")

  def multiRequestedArea(claims: List[Claim]): Int = {
    val allClaimedCoords = claims.foldLeft(List[(Int, Int)]()) { case (accum, claim) =>
      areaFromClaim(claim) ::: accum
    }
    val multiRequestedCoords = allClaimedCoords.diff(allClaimedCoords.distinct).distinct

    multiRequestedCoords.size
  }

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

  // Ridiculously inefficient... 🤢🤢🤢🤢🤢🤢🤢🤢🤢🤢🤢🤢:-/
  def areaFromClaim(claim: Claim): List[(Int, Int)] = (for {
      x <- claim.x until (claim.x + claim.dimensions.width)
      y <- claim.y until (claim.y + claim.dimensions.height)
    } yield (x, y)).toList.distinct

  def uniqueClaim(claims: List[Claim]): Option[Claim] = claims.find { claim =>
      !claims.filterNot(_ == claim).exists { intersects(claim, _) }
    }

  def intersects(claim1: Claim, claim2: Claim): Boolean =
    areaFromClaim(claim1).intersect(areaFromClaim(claim2)).nonEmpty
}
