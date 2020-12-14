import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val lines = file.getLines.toList
    val start = lines.head.toInt
    val best = lines.last.split(",").filter(_ != "x").map(_.toLong).map(v => (v, v - start % v)).minBy(_._2)
    println(best._1 * best._2)
    file.close
  }

}
