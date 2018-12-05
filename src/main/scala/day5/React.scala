import scala.io.Source

object React extends App {
    implicit class RichChar(x: Char) {
        def switchCase() : Char =
            if (x.isUpper) x.toLower else x.toUpper
    }
    val f = Source.fromFile("input_day5.txt")
    val protein = f.mkString.toList.dropRight(1)
    f.close()
    val react : List[Char] => List[Char] = {
        case x :: Nil => List(x)
        case x :: xs => {
            val rest = react(xs)
            if (x.switchCase == rest.head) rest.tail else x :: rest
        }
        case Nil => Nil
    }
    println(react(protein))
}