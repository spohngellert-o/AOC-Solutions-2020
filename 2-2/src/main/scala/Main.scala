import scala.io.Source

object Main {
  class asInt(b: Boolean) {
    def toInt = if(b) 1 else 0
  }

  implicit def convertBooleanToInt(b: Boolean) = new asInt(b)

  def main(args: Array[String]): Unit = {
    val filename = "input"
    var pwds = Array[(Int, Int, Char, String)]()
    for (line <- Source.fromFile(filename).getLines) {
      pwds = pwds :+ readLine(line)
    }
    val valid = pwds.map(tpl => tpl match {
      case (mn, mx, c, pw) => {
        val pwca = pw.toCharArray
        pwca(mn-1) == c ^ pwca(mx-1) == c
      }
      case _ => false
    })
    System.out.println(valid.foldLeft(0)(_+_.toInt))

  }
  def readLine(line: String): (Int, Int, Char, String) = {
    val split1 = line.split("-")
    val v1 = split1(0).toInt
    val split2 = split1(1).split(" ", 2)
    val v2 = split2(0).toInt
    val split3 = split2(1).split(": ")
    val v3 = split3(0).head
    (v1, v2, v3, split3(1))

  }
}
