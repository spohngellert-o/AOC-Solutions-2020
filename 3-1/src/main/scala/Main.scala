import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    System.out.println(Source.fromFile("input").getLines.foldLeft((0, 0))({
      case ((ind, count), line) => (ind+3, count + (if (line.charAt(ind % line.length) == '#') 1 else 0))
    })._2)
  }

}
