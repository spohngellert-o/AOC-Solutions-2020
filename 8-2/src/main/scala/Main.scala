import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val instrs = file.getLines.map(s => (s.split(" ").head, s.split(" ").last.toInt)).toList
    val bad_ind = instrs.foldLeft((0, -1))({case ((cur_ind, bad_ind), (instr, num)) => {
      if (bad_ind > -1) (cur_ind+1, bad_ind)
      else {
        instr match {
          case "jmp" => (cur_ind + 1, if (terminates(0, Set.empty, instrs.updated(cur_ind, ("nop", num)))) cur_ind else bad_ind)
          case "nop" => (cur_ind + 1, if (terminates(0, Set.empty, instrs.updated(cur_ind, ("jmp", num)))) cur_ind else bad_ind)
          case _ => (cur_ind + 1, bad_ind)
        }
      }
    }})._2
    println(getAccAtInfOrEnd(0, 0, Set.empty, instrs.updated(bad_ind, instrs(bad_ind)._1 match {
      case "jmp" => ("nop", instrs(bad_ind)._2)
      case "nop" => ("jmp", instrs(bad_ind)._2)
    })))
    file.close()
  }

  def getAccAtInfOrEnd(acc: Int, ind: Int, seen: Set[Int], instrs: List[(String, Int)]): Int = {
    if (ind == instrs.length - 1 || (seen contains ind)) acc
    else instrs(ind)._1 match {
      case "nop" => getAccAtInfOrEnd(acc, ind+1, seen + ind, instrs)
      case "acc" => getAccAtInfOrEnd(acc + instrs(ind)._2, ind+1, seen + ind, instrs)
      case "jmp" => getAccAtInfOrEnd(acc, ind + instrs(ind)._2, seen + ind, instrs)
    }
  }

  def terminates(ind: Int, seen: Set[Int], instrs: List[(String, Int)]): Boolean = {
    if (seen contains ind) false
    else if(ind == instrs.length - 1) true
    else instrs(ind)._1 match {
      case "nop" => terminates(ind+1, seen + ind, instrs)
      case "acc" => terminates(ind+1, seen + ind, instrs)
      case "jmp" => terminates(ind + instrs(ind)._2, seen + ind, instrs)
    }
  }

}
