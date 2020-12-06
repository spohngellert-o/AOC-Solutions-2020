import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val groups = file.mkString.split("\n\n")
    println(groups.map(g => g.filter(c => "^[a-z]$".r.matches(c.toString)).toSet.size).sum)
    file.close()
  }
}
