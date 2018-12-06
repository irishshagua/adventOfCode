package ie.irishshagua.advent_of_code

import scala.io.Source

object Day6 extends App {

  val ValidCoord = "(\\d+),\\s(\\d+)".r

  type Distance = Int
  type Identifier = String
  type NamedLocation = (Location, Identifier)
  type Ownership = (Location, Option[Identifier])
  case class Location(x: Int, y: Int)
  case class Grid(width: Int, height: Int)


  require({
    val coords = List(
      (Location(1, 1), "A"),
      (Location(1, 6), "B"),
      (Location(8, 3), "C"),
      (Location(3, 4), "D"),
      (Location(5, 5), "E"),
      (Location(8, 9), "F")
    )

    val grid = toGrid(coords)
    val ownership = buildOwnership(grid, coords)

    println(grid)
    println(ownership)
    gridPrinter(ownership)

    false
  })

  val input = Source.fromResource("day6/input").getLines()
    .map(toCoord) // deserialize
    .collect { case Some(c) => c } // exclude shite data
    .toSeq.sortBy { c => (c.x, c.y) } // order asc
    .zipWithIndex

  def toCoord(value: String): Option[Location] = value match {
    case ValidCoord(x, y) => Some(Location(x.toInt, y.toInt))
    case _ => None
  }

  def toGrid(input: Seq[NamedLocation]): Grid =
    Grid(input.maxBy(_._1.x)._1.x, input.maxBy(_._1.y)._1.y)

  def buildOwnership(grid: Grid, destinations: Seq[NamedLocation]): Seq[Ownership] = {
    for {
      x <- grid.width until 0 by -1
      y <- grid.height until 0 by -1
    } yield {
      val loc = Location(x, y)
      (loc, deriveOwnership(loc, destinations))
    }
  }

  def deriveOwnership(loc: Location, destinations: Seq[NamedLocation]): Option[String] = {
    val claims = destinations.foldLeft(Map[Identifier, Distance]()) { case (results, destination) => {
      results + (destination._2 -> manhattanDistance(loc, destination._1))
    }}

    if (loc == Location(3, 4)) println(claims)

    val m: Map[Int, List[String]] = claims.groupBy(_._2).mapValues(_.keys.toList)
    if (m.nonEmpty) {
      m.minBy(_._1)._2 match {
        case hd :: Nil => Some(hd)
        case _ => None
      }
    } else None
  }

  def manhattanDistance(loc1: Location, loc2: Location): Int =
    Math.abs(loc1.x - loc2.x) + Math.abs(loc1.y - loc2.y)

  def gridPrinter(ownership: Seq[Ownership]): Unit = {
    val a = ownership
      .groupBy(_._1.y)
      .toSeq
      .sortBy(_._1)
      .foreach { case (_, owners) =>
        owners.reverse.foreach { o =>
            print(o._2.getOrElse("."))
          }
        println()
      }
  }
}
