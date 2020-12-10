import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val joltages = file.getLines.map(_.toInt).toList
    println(getTotalArrangements(joltages, 0, joltages.max))
    file.close
  }
  def getTotRec(joltages: List[Int], cur: Int, mv:Int): Long = {
    if (cur == mv) 1
    else {
      val launchingPoints = (cur+1 to cur+3).filter(joltages contains _)
      launchingPoints.map(getTotRec(joltages, _, mv)).sum
    }
  }

  def getTotalArrangements(joltages: List[Int], cur: Int, mv:Int): Long = {
    val threeGaps = joltages.sorted.foldLeft((0, List[Int]()))({case ((p, gaps), v) => {
      if (v-3 == p) {
        (v, gaps :+ v)
      }
      else (v, gaps)
    }})._2 :+ mv+3
    threeGaps.foldLeft((0, 1:Long))({case ((prev, acc), v) => {
      (v, acc * getTotRec(joltages, prev, v-3))
    }
    })._2
  }
}
