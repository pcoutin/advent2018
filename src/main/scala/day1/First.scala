import scala.io.Source

object First extends App {
    val file = Source.fromFile("input_day1.txt")
    val lines = file.getLines
    val freq = lines.map(_.toInt).foldLeft(0)(_ + _)
    println(freq)
    
    file.close()
}