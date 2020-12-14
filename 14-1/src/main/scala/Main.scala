import scala.collection.immutable.HashMap
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val mem = file.getLines.toList.foldLeft((HashMap[Int, Long](), Map[Int, Char]()))({case ((map, mask), l) => {
      if (l.startsWith("mask")) {
        val num = l.split("mask = ").last
        (map, num.zipWithIndex.filter(_._1 != 'X').map(x => (num.length - x._2 - 1, x._1)).toMap)
      }
      else {
        val addr = l.split("\\[").last.split("]").head.toInt
        val orig = l.split("= ").last.toLong
        val origBin = toBinary(orig)
        val masked = mask.foldLeft(orig)({case (acc, (ind, char)) => {
          char match {
          case '1' => if (origBin(origBin.length - ind - 1) == char) acc else acc + Math.pow(2, ind).toLong
          case '0' => if (origBin(origBin.length - ind - 1) == char) acc else acc - Math.pow(2, ind).toLong
        }
        }})
        (map.updated(addr, masked), mask)
      }
    }})._1
    println(mem.foldLeft(0.toLong)({case (acc, (ind, v)) => acc + v}))
    file.close
  }

  def toBinary(n: Long): String = {
    n.toBinaryString.reverse.padTo(64, '0').reverse
  }

}
