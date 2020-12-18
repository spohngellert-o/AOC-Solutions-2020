
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val lines = file.getLines.toList
    println(lines.map(evaluateMath).sum)
    file.close
  }

  def evaluateMath(line: String): Long = {
    val (fvs, fops, _) = line.foldLeft(List((0.toLong, 0)), List('+'), 0)({case ((vs, ops, level), v) => {
      if (v == ' ') (vs, ops, level)
      else if (v.isDigit) {
        if (ops.head == '*') ((v.asDigit.toLong, level) :: vs, ops, level)
        else ((doOp(vs.head._1, v.asDigit.toLong, ops.head), level) :: vs.tail, ops.tail, level)
      }
      else if (v == '(') ((0.toLong, level+1) :: vs, '+' :: ops, level+1)
      else if (v == ')') {
        val combiners = vs.takeWhile(_._2 == level)
        val (nvs, nops, nlevel) = ((comb(combiners, ops.take(combiners.length  - 1)), level-1) :: vs.slice(combiners.length, vs.length), ops.slice(combiners.length-1, ops.length), level-1)
        if (nops.head == '*') (nvs, nops, nlevel)
        else ((doOp(nvs.head._1, nvs.tail.head._1, nops.head), nlevel) :: nvs.tail.tail, nops.tail, nlevel)
      }
      else (vs, v :: ops, level)
    }
    })
    comb(fvs, fops)
  }
  def doOp(v1: Long, v2: Long, op: Char): Long = {
    op match {
      case '+' => v1 + v2
      case '*' => v1 * v2
    }
  }
  def comb(vs: List[(Long, Int)], ops: List[Char]): Long = {
    vs.tail.zip(ops).foldLeft(vs.head._1) {case (acc, ((v, _), op)) => {
      doOp(acc, v, op)
    }}
  }
}