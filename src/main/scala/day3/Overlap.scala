import scala.io.Source
import scala.collection.mutable

object Overlap {
    def overlapX(aA: Rectangle, bB: Rectangle) : Boolean = {
        val x = aA.x 
        val y = aA.x + aA.w
        val a = bB.x
        val b = bB.x + bB.w
        a > x && a < y ||
        b < y && b > x ||
        a <= x && b >= y
    }
    
    def overlapY(aA: Rectangle, bB: Rectangle) : Boolean = {
        val x = aA.y
        val y = aA.y + aA.h
        val a = bB.y 
        val b = bB.y + bB.h
        a > x && a < y ||
        b < y && b > x ||
        a <= x && b >= y
    }
    
    def overlap(a: Rectangle, b: Rectangle) : Boolean = overlapX(a, b) && overlapY(a, b)

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

        val overlaps = mutable.Map[Rectangle, Boolean]().withDefaultValue(false)

        for (lineA <- lines) {
            for (lineB <- lines) {
                if (lineA != lineB && this.overlap(lineA, lineB)) {
                    overlaps(lineA) = true
                    overlaps(lineB) = true
                }
            }
        }
        for (line <- lines) {
            if (!overlaps(line)) println(line)
        }
    }
    def e() = main(new Array[String](0)) 
}