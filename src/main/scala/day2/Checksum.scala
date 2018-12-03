import scala.io.Source

object Checksum {
    def main(args: Array[String]) : Unit = {
        val file = Source.fromFile("input_day2.txt")
        val lines = file.getLines.toList
        file.close()

        var total_twos = 0
        var total_threes = 0

        for (line <- lines) {
            val m = line.foldLeft (Map[Char, Int]()) { (ms : Map[Char, Int], cur) =>
                ms + (cur -> (ms.getOrElse(cur, 0) + 1))
            }
            val twos = !m.filter(_._2 == 2).isEmpty
            val threes = !m.filter(_._2 == 3).isEmpty

            if (twos) total_twos += 1
            if (threes) total_threes += 1
        }
        println(total_twos * total_threes)
    }
    def e() = main(new Array[String](0))
}