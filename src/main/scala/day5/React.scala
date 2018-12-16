/*
 * needs enlarged stack size
 * export SBT_OPTS="-Xss16M"
 * set SBT_OPTS=-Xss16M
 */

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
            if (rest != Nil && x.switchCase == rest.head) {
                rest.tail
            } else {
                x :: rest
            }
        }
        case Nil => Nil
    }

    val part1 = react(protein).length
    println(s"part1=$part1")

    val letters = protein.map(_.toLower).toSet
    def reactExChar(c: Char) = react(protein.filter(_.toLower != c)).length
    val part2 = letters
        .map(reactExChar)
        .reduceLeft((a, b) => if (a < b) a else b)
    println(s"part2=$part2")
}
React.main(Array[String]())
