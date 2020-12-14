import scala.collection.immutable.HashMap
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val mem = file.getLines.toList.foldLeft((HashMap[Long, Long](), Map[Int, Char]()))({case ((map, mask), l) => {
      if (l.startsWith("mask")) {
        val num = l.split("mask = ").last
        (map, num.zipWithIndex.filter(_._1 != '0').map(x => (num.length - x._2 - 1, x._1)).toMap)
      }
      else {
        val addr = l.split("\\[").last.split("]").head.toLong
        val orig = l.split("= ").last.toLong
        val origBin = toBinary(addr)
        val masked = mask.foldLeft(addr)({case (acc, (ind, char)) => {
          char match {
            case '1' => if (origBin(origBin.length - ind - 1) == char) acc else acc + Math.pow(2, ind).toLong
            case _ => acc
          }
        }})
        val maskedBin = toBinary(masked)
        val xMasked = mask.foldLeft(List(BigInt(masked)))({case (acc, (ind, char)) => {
          char match {
            case 'X' => acc.flatMap(v => {
              val isOne = maskedBin(maskedBin.length - ind - 1) == '1'
              if (isOne) List(v, v-Math.pow(2, ind).toLong)
              else List(v, v+Math.pow(2, ind).toLong)
            })
            case _ => acc
          }
        }})
        (xMasked.foldLeft(map)((acc, v) => acc.updated(v.toLong, orig)), mask)
      }
    }})._1
    println(mem.foldLeft(BigInt(0))({case (acc, (addr, v)) => acc + v}))
    file.close
  }

  def toBinary(n: Long): String = {
    n.toBinaryString.reverse.padTo(64, '0').reverse
  }

}
