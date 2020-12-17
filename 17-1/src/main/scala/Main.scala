import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val coordMap = file.getLines().zipWithIndex.toArray.flatMap {
      case (l, y) => l.zipWithIndex.map({case (c, x) => (x, y, 0) -> (if(c == '#') 1 else 0)})
    }.toMap
    val nthCm = getNthCycle(coordMap, 6)
    println(nthCm.values.sum)
    file.close
  }

  def getNthCycle(coordMap: Map[(Int, Int, Int), Int], target: Int, cur: Int=0): Map[(Int, Int, Int), Int] = {
    if (cur == target) coordMap
    else {
      val checkCoords = coordMap.keys.flatMap(getNeighbors).toSet
      val nm = checkCoords.foldLeft(coordMap)((cm, coord) => {
        val cs = cm.getOrElse(coord, 0)
        val activeNeighbors = getNeighbors(coord).map(nc => coordMap.getOrElse(nc, 0)).sum
        if (cs == 0) {
          if (activeNeighbors == 3) cm.updated(coord, 1) else cm.updated(coord, 0)
        }
        else {
          if  (activeNeighbors == 3 || activeNeighbors == 4) cm.updated(coord, 1) else cm.updated(coord, 0)
        }
      })
      getNthCycle(nm, target, cur+1)
    }
  }
  def getNeighbors(coord: (Int, Int, Int)): List[(Int, Int, Int)] = {
    coord match {
      case (x, y, z) => (x - 1 to x + 1).toList.flatMap(cx => {
        (y - 1 to y + 1).toList.flatMap(cy => {
          (z - 1 to z + 1).map(cz => (cx, cy, cz))
        })
      })
    }
  }
}
