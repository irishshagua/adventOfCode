package ie.irishshagua.advent_of_code

import scala.annotation.tailrec
import scala.io.Source

case class CountAccumulation(twoLetterDupes: Int = 0, threeLetterDupes: Int = 0)

object Day2 extends App {

  require(retrieveCounts(List("abcdef")).twoLetterDupes == 0); require(retrieveCounts(List("abcdef")).threeLetterDupes == 0)
  require(retrieveCounts(List("bababc")).twoLetterDupes == 1); require(retrieveCounts(List("bababc")).threeLetterDupes == 1)
  require(retrieveCounts(List("abbcde")).twoLetterDupes == 1); require(retrieveCounts(List("abbcde")).threeLetterDupes == 0)
  require(retrieveCounts(List("abcccd")).twoLetterDupes == 0); require(retrieveCounts(List("abcccd")).threeLetterDupes == 1)
  require(retrieveCounts(List("aabcdd")).twoLetterDupes == 1); require(retrieveCounts(List("aabcdd")).threeLetterDupes == 0)
  require(retrieveCounts(List("abcdee")).twoLetterDupes == 1); require(retrieveCounts(List("abcdee")).threeLetterDupes == 0)
  require(retrieveCounts(List("ababab")).twoLetterDupes == 0); require(retrieveCounts(List("ababab")).threeLetterDupes == 1)

  require({
    val inputs = List("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
    val letterCounts = retrieveCounts(inputs)
    val localChecksum = letterCounts.twoLetterDupes * letterCounts.threeLetterDupes

    localChecksum == 12
  })

  require({
    val inputs = List("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
    val relations = relatedWords(inputs)

    relations.size == 1 && relations.head == ("fguij", "fghij")
  })

  val input = Source.fromResource("day2/input").getLines().toList
  val letterCounts = retrieveCounts(input)
  val checksum = letterCounts.twoLetterDupes * letterCounts.threeLetterDupes
  println(s"Checksum: $checksum")

  val relations = relatedWords(input).map(intersect)
  println("Relations: " + relations)

  @tailrec
  def retrieveCounts(inputs: List[String], accum: CountAccumulation = CountAccumulation()): CountAccumulation = inputs match {
    case Nil => accum
    case hd :: tail =>
      val counts: Map[Char, Int] = dupeCounts(hd.toList)
      retrieveCounts(tail, accum.copy(
        twoLetterDupes = if (counts.exists { _._2 == 2 }) accum.twoLetterDupes + 1 else accum.twoLetterDupes,
        threeLetterDupes = if (counts.exists { _._2 == 3 }) accum.threeLetterDupes + 1 else accum.threeLetterDupes
      ))
  }

  @tailrec
  def dupeCounts(letters: List[Char], accum: Map[Char, Int] = Map.empty): Map[Char, Int] = letters match {
    case Nil => accum
    case hd :: tail => dupeCounts(tail, accum.updated(hd, accum.getOrElse(hd, 0) + 1))
  }

  def relatedWords(words: List[String]): List[(String, String)] = {
    words.foldLeft(List[(String, String)]()) { case (accum, word) =>
      words.filter { it => it != word && areSimilar(it, word) && !accum.exists { _._2 == it } }.map { (_, word) } ::: accum
    }
  }

  def areSimilar(word1: String, word2: String): Boolean = word1.zip(word2).foldLeft(0) {
    case (accum, (l1, l2)) => if (l1 == l2) accum else accum + 1
  } < 2

  def intersect(wordPair: (String, String)): String =
    wordPair._1.zip(wordPair._2).filter { case (a, b) => a == b }.map { _._1 }.mkString("")
}
