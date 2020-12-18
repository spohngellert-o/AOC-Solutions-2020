
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val lines = file.getLines.toList
    println(lines.map(evaluateMath).sum)
    file.close
  }

  def evaluateMath(line: String): Long = {
    line.foldLeft(List(0.toLong), List('+'))({case ((vs, ops), v) => {
      if (v == ' ') (vs, ops)
      else if (v.isDigit) {
        (doOp(vs.head, v.asDigit.toLong, ops.head) :: vs.tail, ops.tail)
      }
      else if (v == '(') (0 :: vs, '+' :: ops)
      else if (v == ')') (doOp(vs.head, vs.tail.head, ops.head) :: vs.tail.tail, ops.tail)
      else (vs, v :: ops)
    }
    })._1.head
  }
  def doOp(v1: Long, v2: Long, op: Char): Long = {
    op match {
      case '+' => v1 + v2
      case '*' => v1 * v2
    }
  }
}
