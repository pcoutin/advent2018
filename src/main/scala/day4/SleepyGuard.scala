import scala.io.Source
import java.time.LocalDateTime
import java.time.Duration
import scala.collection.mutable

sealed trait GuardEntry
final case class BeginShift(guardId: Int) extends GuardEntry
final case class FallAsleep() extends GuardEntry
final case class WakeUp() extends GuardEntry

object SleepyGuard extends App {
    val file = Source.fromFile("input_day4.txt")
    val DateRE = raw"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)".r
    val ClockInRE = "Guard #(\\d+) begins shift".r
    val processEntry : String => (LocalDateTime, GuardEntry) = s => {
        val DateRE(year, month, day, hh, mm, actionS) = s
        val dt = LocalDateTime.of(year.toInt, month.toInt, day.toInt, hh.toInt, mm.toInt)

        val action : GuardEntry = actionS match {
            case ClockInRE(guardId) => BeginShift(guardId.toInt)
            case "wakes up" => WakeUp()
            case "falls asleep" => FallAsleep()
        }
        (dt, action)
    }
    val lines = file
        .getLines
        .toList
        .map(processEntry)
        .sortWith((a : (LocalDateTime, GuardEntry), b) => a._1.compareTo(b._1) < 0)
    file.close()

    val guards = lines.filter({
        case (_, BeginShift(_)) => true
        case _ => false 
    }).map({
        case (_, BeginShift(guardId)) => guardId
        case _ => throw new Exception("why")
    }).toSet
    println(s"guards $guards")

    def getSleepiestGuard(lines : List[(LocalDateTime, GuardEntry)]) = {

        val guardSleepSeconds = mutable.Map[Int, Int]().withDefaultValue(0)
        var currentGuard:Int = -1
        var lastSleepTime : LocalDateTime = null
        for (line <- lines) {
            line match {
                case (_, BeginShift(id)) => {
                    currentGuard = id
                }
                case (t, FallAsleep()) => {
                    lastSleepTime = t
                }
                case (t, WakeUp()) => {
                    val timePassed = Duration.between(lastSleepTime, t).getSeconds().toInt
                    guardSleepSeconds(currentGuard) += timePassed
                    lastSleepTime = null
                }
            }
        }
        var maxSecsSlept = -1
        var maxSleepGuard = -1
        for (guard <- guardSleepSeconds) {
            if (guard._2 > maxSecsSlept) {
                maxSecsSlept = guard._2
                maxSleepGuard = guard._1
            }
        }
        println(s"guard $maxSleepGuard slept $maxSecsSlept")
        (maxSleepGuard, maxSecsSlept)
    }
    def minuteOfDate(t : LocalDateTime) : Int = {
        t.getHour() * 60 + t.getMinute()
    }
    def getGuardSleepiestMinute(guardId: Int) : (Int, Int) = {
        // Only the minutes from 00:00 - 00:59 matter, but this count
        // all minutes of the day..
        val dayMins = new Array[Int](24*60)
        var currentGuard:Int = -1
        var lastSleepTime : LocalDateTime = null
        for (line <- lines) {
            line match {
                case (_, BeginShift(id)) => {
                    currentGuard = id
                }
                case (t, FallAsleep()) => {
                    lastSleepTime = t
                }
                case (t, WakeUp()) => {
                    for (min <- minuteOfDate(lastSleepTime) to minuteOfDate(t)) {
                        if (currentGuard == guardId) {
                            dayMins(min) += 1
                        }
                    }
                    lastSleepTime = null
                }
            }
        }
        var maxMinutes = Int.MinValue
        var maxId = -1
        for (min <- dayMins.indices) {
            if (dayMins(min) > maxMinutes) {
                maxMinutes = dayMins(min)
                maxId = min
            }
        }
        val zzhour = maxId / 60
        val zzmin = maxId % 60
        (zzmin, maxMinutes)
    }
    val sleepiestGuard = getSleepiestGuard(lines)._1
    println(s"guard $sleepiestGuard slept (minute,amt) ${getGuardSleepiestMinute(sleepiestGuard)}")
    val sg = guards.toList.map(g => g -> getGuardSleepiestMinute(g)).toMap
    val sleepiestMin = sg.reduceLeft((a, b) => if (a._2._2 > b._2._2) a else b)
    println(sleepiestMin)
}