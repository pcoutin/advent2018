/*
 * Part One is basically a topological sort.
 * Part Two is a minimum cost path that goes through all vertices(steps)
 */
import scala.io.Source

type M = SortedMap[Char, Set[Char]]

// take the first step that has no dependencies, return it along
// with the map passed in, but without the step
def nextStep(m: M): (Option[Char], M) = {
    val empties = m.filter{case (k, v) => v.isEmpty}
    if (empties.isEmpty) {
        (None, m)
    } else {
        val c = empties.head._1
        val r = m.map({case (k, v) => (k, v.filter(_ != c))})
            .filter(_._1 != c)
        (Some(c), r)
    }
}

// This could be tail recursive, but then the list would be in reverse order.
// What if there was something like
// A -> (A -> (Option[B], A)) -> List(B)
// ...that would keep applying the function in the 2nd parameter until it
// returned (None,_)? Or something to keep extracting and accumulating values
// from a collection until something is met.
def solveDeps(m: M) : List[Char] = nextStep(m) match {
    case (Some(c), m_) => c :: solveDeps(m_)
    case _ => Nil
}

object Depend extends App {
    val Re = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
    def processEntry(s: String): (Char, Char) = {
        val Re(dep, child) = s
        (child.head, dep.head)
    }

    val file = Source.fromFile("input/input_day7.txt")
    val lines = file
        .getLines
        .map(processEntry)
        .toList
    file.close()

    def updateMap(m: M, kv: (Char, Char)): M = {
        val k = kv._1
        val v = kv._2
        val s = m.getOrElse(k, Set[Char]()) + v
        m + (k -> s)
    }
    val initMap = (SortedMap(): M) ++ ('A' to 'Z').map(_ -> Set[Char]())
    val stepDeps = lines.foldLeft(initMap)(updateMap)

    println(s"res1=${solveDeps(stepDeps) mkString ""}")
}
Depend.main(Array[String]())
