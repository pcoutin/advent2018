import scala.io.Source


object Second extends App {
    val file = Source.fromFile("input_day1.txt")
    val lines = file.getLines.map(_.toInt).toList
    file.close()
    var foundFreqs : Set[Int] = Set()
    var firstDupe : Option[Int] = None
    var frequency = 0
    while (firstDupe == None) {
        for (reading <- lines) {
            frequency += reading
            if (foundFreqs(frequency)) {
                firstDupe = Some(frequency)
                println(s"found dupe $frequency")
            }
            foundFreqs += frequency
        }
    }
}
/*
case class StateBaggage(freq: Int, lines: List[Int], visited: Set[Int], firstDupe: Option[Int])

object Second {

    def freqOut(s: StateBaggage) : StateBaggage = {
        val foldFunc : (List[Int], Int) => List[Int] =
            (acc, i) => (acc.head + i) :: acc
        val freqs = (s.lines).foldLeft(List(s.freq))(foldFunc).reverse

        val initial : (Set[Int], Option[Int]) = (s.visited, s.firstDupe)
        val dup = freqs.foldLeft(initial){ case ((found, firstDup), current) => {
            if (firstDup != None) {
                (found, firstDup)
            } else if (found(current)) {
                
                println(s"eureka $current")
                (found, Some(current))
            } else {
                (found + current, firstDup)
            }
        }}
        StateBaggage(freqs.last, s.lines, dup._1, dup._2)
    }

    def magic(s: StateBaggage) : StateBaggage = {
        val retval = freqOut(s)
        if (retval.firstDupe == None) {
            freqOut(retval)
        } else {
            retval
        }
    }

    def main(args: Array[String]) : Unit = {
        val file = Source.fromFile(raw"C:\Users\a\AppData\Roaming\input")
        val lines = file.getLines.map(_.toInt).toList
        file.close()

        magic(StateBaggage(0, lines, Set(), None))

    }
} */