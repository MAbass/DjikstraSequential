package sn.esp

import scala.annotation.tailrec

//Code source: https://rosettacode.org/wiki/Dijkstra%27s_algorithm#Scala
object MainClass {

  type Path[Key] = (Double, List[Key])

  @tailrec
  def Dijkstra[Key](lookup: Map[Key, List[(Double, Key)]], fringe: List[Path[Key]], dest: Key, visited: Set[Key]): Path[Key] = fringe match {
    case (dist, path) :: fringe_rest => path match {
      case key :: path_rest =>
        if (key == dest) (dist, path.reverse)
        else {
          val paths = lookup(key).flatMap { case (d, key) => if (!visited.contains(key)) List((dist + d, key :: path)) else Nil }
          val sorted_fringe = (paths ++ fringe_rest).sortWith { case ((d1, _), (d2, _)) => d1 < d2 }
          Dijkstra(lookup, sorted_fringe, dest, visited + key)
        }
    }
    case Nil => (0, List())
  }

  def main(x: Array[String]): Unit = {
    val lookup = Map(
      "A" -> List((4.0, "B"), (8.0, "C")),
      "B" -> List((21.0, "E"), (4.0, "A"), (18.0, "D"), (7.0, "C")),
      "C" -> List((8.0, "A"), (10.0, "D"), (7.0, "B")),
      "D" -> List((18.0, "B"), (10.0, "C"), (15.0, "E"), (31.0, "G"), (12.0, "F")),
      "E" -> List((21.0, "B"), (15.0, "D"), (10.0, "F"), (17.0, "G")),
      "F" -> List((25.0, "C"), (7.0, "G"), (10.0, "E")),
      "G" -> List((7.0, "F"), (17.0, "E"), (31.0, "D")),
    )
    val res = Dijkstra[String](lookup, List((0, List("B"))), "F", Set())
    println(res)
  }
}