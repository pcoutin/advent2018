import scala.io.Source

object MagicCompare {
    def main(args: Array[String]) : Unit = {
        val file = Source.fromFile("input_day2.txt")
        val lines = file.getLines.toList
        file.close()
        val len = lines.length
        println(len*len)

        // 62500 - the input is not too large for O(n^2)

        for (lineA <- lines) {
            for (lineB <- lines) {
                // lineA can be lineB, but we only want the one
                // with exactly 1 different character
                val diff = lineA
                    .zip(lineB)
                    .map {case (x, y) => x == y}
                    .count (_ == false)
                if (diff == 1) {
                    println(lineA)
                    println(lineB)
                }
            }
        }
    }
    def e() = main(new Array[String](0))
}