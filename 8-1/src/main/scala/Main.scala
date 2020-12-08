
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val instrs = file.getLines.map(s => (s.split(" ").head, s.split(" ").last.toInt)).toList
    println(getAccAtInf(0, 0, Set(), instrs))
    file.close()
  }

  def getAccAtInf(acc: Int, ind: Int, seen: Set[Int], instrs: List[(String, Int)]): Int = {
    if (seen contains ind) acc
    else instrs(ind)._1 match {
      case "nop" => getAccAtInf(acc, ind+1, seen + ind, instrs)
      case "acc" => getAccAtInf(acc + instrs(ind)._2, ind+1, seen + ind, instrs)
      case "jmp" => getAccAtInf(acc, ind + instrs(ind)._2, seen + ind, instrs)
    }
  }

}
