import scala.io.Source
import scala.math

sealed trait Loc

final case object NoLoc extends Loc {
    override def toString: String = "X"
}
case class Point(x: Int, y: Int) extends Loc {

    def distanceTo(p: Point): Int = {
        val a = math.abs(p.x - this.x)
        val b = math.abs(p.y - this.y)
        a + b // Manhattan distance
    }

    def nearestTo(s: Set[Point]): Loc = {
        val pointsDist = s.map(p => p -> p.distanceTo(this))
        val minPoint = pointsDist.reduceLeft((a, b) =>
                if (a._2 < b._2) a else b)
        val otherMins = pointsDist.filter({
            case (p, dist) => dist == minPoint._2 && p != minPoint._1
        })
        if (otherMins.isEmpty) {
            minPoint._1
        } else {
            NoLoc
        }
    }
}

object Coords extends App {
    val coords = Source.fromFile("input/input_day6.txt")
        .getLines
        .map(_.split(", ").map(_.toInt))
        .map({case Array(x, y) => Point(x, y)})
        .toSet
    println(coords)

    val minX = coords.reduceLeft((a, b) => if (a.x < b.x) a else b).x
    val maxX = coords.reduceLeft((a, b) => if (a.x > b.x) a else b).x
    val minY = coords.reduceLeft((a, b) => if (a.y < b.y) a else b).y
    val maxY = coords.reduceLeft((a, b) => if (a.y > b.y) a else b).y
    
    val asdf = Array.tabulate(maxX, maxY)((x, y) => Point(x,y).nearestTo(coords))
    println(asdf)

    val nearPoints = asdf.reduceLeft((a, b) => a ++ b).toList

    val topCoords = (0 to (maxX+1)).map(x => Point(x, -1))
    val bottomCoords = (0 to (maxX+1)).map(x => Point(x, maxY+1))
    val leftCoords = (0 to (maxY+1)).map(y => Point(-1, y))
    val rightCoords = (0 to (maxY+1)).map(y => Point(maxX+1, y))

    val surroundingCoords = (topCoords ++ bottomCoords ++ leftCoords ++ rightCoords).toSet

    // Any "nearest points" outside the smallest rectangle where all coords are,
    // could be part of an infinite area of nearest points.
    val infAreas = surroundingCoords.map(_.nearestTo(coords)).filter(_ != NoLoc)

    val testCoords = coords.filter(x => infAreas.contains(x) == false)
    val areas = testCoords.map(c => c -> nearPoints.filter(x => x == c).length)
    println(areas)

    val bigarea = areas.reduceLeft((a, b) => if (a._2 > b._2) a else b)
    println(s"ans1=$bigarea")

    val totDist = Array.tabulate(maxX, maxY)((x, y) =>
            coords.toList.map(c => c.distanceTo(Point(x, y))).reduceLeft(_ + _))

    val distPoints = totDist.reduceLeft((a, b) => a ++ b).toList.filter(x => x < 10000)
    println(s"ans2=${distPoints.length}")
}

Coords.main(Array[String]())
