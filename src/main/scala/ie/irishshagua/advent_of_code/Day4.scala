package ie.irishshagua.advent_of_code

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import scala.annotation.tailrec
import scala.io.Source

case class GuardSleepReport(id: Int, date: LocalDate, snoozeyTime: Set[Int])

trait ShiftActions
case class GuardStarts(night: LocalDate, guardId: Int) extends ShiftActions
case class FallsAsleep(minute: Int) extends ShiftActions
case class WakesUp(minute: Int) extends ShiftActions

object Day4 extends App {

  val ShiftStartPattern = "\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})\\] Guard #(\\d+) begins shift".r
  val SleepStartPattern = "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:(\\d{2})\\] falls asleep".r
  val SleepEndPattern = "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:(\\d{2})\\] wakes up".r
  val DateTimeFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm")


  require({
    val testData = List(
      "[1518-11-01 00:00] Guard #10 begins shift",
      "[1518-11-01 00:05] falls asleep",
      "[1518-11-01 00:25] wakes up",
      "[1518-11-01 00:30] falls asleep",
      "[1518-11-01 00:55] wakes up",
      "[1518-11-01 23:58] Guard #99 begins shift",
      "[1518-11-02 00:40] falls asleep",
      "[1518-11-02 00:50] wakes up",
      "[1518-11-03 00:05] Guard #10 begins shift",
      "[1518-11-03 00:24] falls asleep",
      "[1518-11-03 00:29] wakes up",
      "[1518-11-04 00:02] Guard #99 begins shift",
      "[1518-11-04 00:36] falls asleep",
      "[1518-11-04 00:46] wakes up",
      "[1518-11-05 00:03] Guard #99 begins shift",
      "[1518-11-05 00:45] falls asleep",
      "[1518-11-05 00:55] wakes up"
    )

    val testReports = generateReports(testData.sorted.flatMap(toShiftAction))
    val testBreakdown = reportsPerGuard(testReports)
    val result1 = calcPart1Result(testBreakdown)
    val result2 = calcPart2Result(testBreakdown)

    result1.contains(240) && result2.contains(4455)
  })

  val input = Source.fromResource("day4/input").getLines().toList.sorted.flatMap(toShiftAction)
  val reports = generateReports(input)
  val breakdown = reportsPerGuard(reports)
  println(s"Day 4 Part 1 Result: ${calcPart1Result(breakdown)}")
  println(s"Day 4 Part 2 Result: ${calcPart2Result(breakdown)}")


  def toShiftAction(report: String): Option[ShiftActions] = report match {
    case ShiftStartPattern(dateTime, guardId) =>
      val dt = LocalDateTime.parse(dateTime, DateTimeFormat)
      val date = if (dt.getHour == 23) dt.toLocalDate.plusDays(1) else dt.toLocalDate
      Some(GuardStarts(date, guardId.toInt))
    case SleepStartPattern(time) => Some(FallsAsleep(time.toInt))
    case SleepEndPattern(time) => Some(WakesUp(time.toInt))
    case _ => None
  }

  @tailrec
  def generateReports(actions: List[ShiftActions], inflightReport: Option[GuardSleepReport] = None, completedReports: List[GuardSleepReport] = Nil, sleepStarted: Option[Int] = None): List[GuardSleepReport] = actions match {
    case Nil => inflightReport.toList ::: completedReports
    case hd :: tail => hd match {
      case GuardStarts(date, id) => generateReports(tail, Some(GuardSleepReport(id, date, Set())), inflightReport.toList ::: completedReports)
      case FallsAsleep(minute) => generateReports(tail, inflightReport, completedReports, Some(minute))
      case WakesUp(minute) => generateReports(tail, inflightReport.map { rep => rep.copy(snoozeyTime = sleepStarted.map(_ until minute).map(_.toSet).toSet.flatten ++ rep.snoozeyTime) }, completedReports, None)
    }
  }

  def reportsPerGuard(reports: List[GuardSleepReport]): Map[Int, List[GuardSleepReport]] =
    reports.groupBy(_.id)

  def sleepiestGuard(reports: Map[Int, List[GuardSleepReport]]): Option[Int] =
    reports.mapValues(l => l.map(_.snoozeyTime.size).sum).toSeq.sortBy(_._2).reverse.headOption.map(_._1)

  def mostFrequentSleepingMinute(reports: List[GuardSleepReport]): Option[(Int, Int)] = {
    val grouped = reports.flatMap(_.snoozeyTime).groupBy(identity)
    if (grouped.nonEmpty) {
      Some(
        grouped.maxBy(_._2.size) match {
          case (min, lsMins) => (min, lsMins.size)
        }
      )
    } else None
  }

  def calcPart1Result(breakdown: Map[Int, List[GuardSleepReport]]): Option[Int] = for {
      guardId <- sleepiestGuard(breakdown)
      reports <- breakdown.get(guardId)
      (sleepingMinute, _) <- mostFrequentSleepingMinute(reports)
    } yield guardId * sleepingMinute

  def calcPart2Result(breakdown: Map[Int, List[GuardSleepReport]]): Option[Int] = breakdown.map {
    case (guardId, gaurdsReports) => (guardId, mostFrequentSleepingMinute(gaurdsReports))
  }
    .collect { case (k, Some(v)) => (k, v) }
    .toSeq
    .sortBy(_._2._2)
    .reverse
    .map { i => (i._1, i._2._1) }
    .headOption
    .map { case (guardId, sleepingMin) =>
      guardId * sleepingMin
    }
}
