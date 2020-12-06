import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val groups = file.mkString.split("\n\n")
    println(groups.map(g => {
      val subgs = g.split("\n")
      subgs.head.map(c => subgs.forall(ans => ans.contains(c))).filter(v => v).size
    }).sum)
    file.close()
  }
}
