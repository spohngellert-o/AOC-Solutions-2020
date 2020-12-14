import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val lines = file.getLines.toList
    val inds = lines.last.split(",").zipWithIndex.filter(_._1 != "x").map({case (v, ind) =>
      (v.toLong, ind % v.toInt)
    }).toList
    println(findValidTime(inds))
    file.close
  }

  def findValidTime(inds: List[(Long, Int)]): BigInt = {
    BigInt(inds.foldLeft(1.toLong)((acc, v) => acc * v._1)) - inds.foldLeft((BigInt(0), 1.toLong))({case ((c, mul), (v, m)) => {
      var cur = c
      while (cur.mod(v) != m) {
        cur += mul
      }
      println(cur)
      (cur, mul * v)
    }
    })._1
  }
}
