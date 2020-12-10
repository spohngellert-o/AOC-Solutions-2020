import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val joltages = file.getLines.map(_.toInt).toList
    val diffs = joltages.sorted.foldLeft((0, 0, 0))({case ((cj, d1, d3), v)=> {
      if (v == cj + 1) (cj + 1, d1+1, d3)
      else if (v == cj + 2) (cj+2, d1, d3)
      else (cj+3, d1, d3+1)
    }
    })
    println(diffs._2 * (diffs._3+1))
    file.close
  }

}
