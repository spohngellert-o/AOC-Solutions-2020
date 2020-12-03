import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val slopes = Array((1, 1), (3, 1), (5, 1) , (7, 1), (1, 2))

    print(slopes.map({case (r, d) => getTreeCount(r, d)}).reduce(_*_))
  }
  def getTreeCount(right: Int, down: Int): Long = {
    Source.fromFile("input").getLines.grouped(down).foldLeft((0, 0))({
      case ((ind, count), lines) => (ind+right, count + (if (lines(0).charAt(ind % lines(0).length) == '#') 1 else 0))
    })._2.toLong
  }

}
