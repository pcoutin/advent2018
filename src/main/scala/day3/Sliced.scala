import scala.io.Source

object Sliced {
    val cloth = Array.ofDim[Int](1000, 1000)

    def main(args: Array[String]): Unit = {
        val file = Source.fromFile("input_day3.txt")

        val magic : Array[Int] => Rectangle = {
            case Array(x, y, w, h) => Rectangle(x, y, w, h)
        }
        val lines = file
            .getLines
            .toList
            .map(_.split("[@,:x\\s]").filter(_ != "").drop(1).map(_.toInt))
            .map(magic)
        file.close()

        for (line <- lines) {
            for (x <- line.x until (line.x + line.w)) {
                for (y <- line.y until (line.y + line.h)) {
                    this.cloth(x)(y) += 1
                }
            }
        }
        var acc = 0
        for (row <- this.cloth) {
            for (col <- row) {
                if (col >= 2) {
                    acc += 1
                }
            }
        }
        println(acc)

    }
    def e() = main(new Array[String](0)) 
}