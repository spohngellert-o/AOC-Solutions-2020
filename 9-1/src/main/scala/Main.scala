import java.util
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    print(file.getLines.foldLeft((-1, 0, List[Int]()))({case ((bad_ind, ind, prev25), v) => {
      if (prev25.length < 25) (bad_ind, ind+1, prev25 :+ v.toInt)
      else if (bad_ind != -1) (bad_ind, ind+1, prev25)
      else if (containsSum(prev25, v.toInt)) (bad_ind, ind+1, prev25.takeRight(24) :+ v.toInt)
      else (v.toInt, ind+1, prev25)
    }})._1)
    file.close
  }
  def containsSum(vals: List[Int], cv: Int): Boolean = {
    vals.exists(v => vals contains cv-v)
  }

}
