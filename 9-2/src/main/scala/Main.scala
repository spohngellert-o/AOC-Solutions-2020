import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val nums = file.getLines.map(_.toLong).toList
    val bad_num = (nums.foldLeft((-1, 0, List[Long]()))({case ((bad_ind, ind, prev25), v) => {
      if (prev25.length < 25) (bad_ind, ind+1, prev25 :+ v)
      else if (bad_ind != -1) (bad_ind, ind+1, prev25)
      else if (containsSum(prev25, v.toInt)) (bad_ind, ind+1, prev25.takeRight(24) :+ v)
      else (v.toInt, ind+1, prev25)
    }})._1)
    println(findContiguousSum(0, 1, bad_num, nums).getOrElse(-1))
    file.close
  }
  def containsSum(vals: List[Long], cv: Int): Boolean = {
    vals.exists(v => vals contains cv-v)
  }

  @tailrec
  def findContiguousSum(start: Int, end: Int, badv: Int, nums: List[Long]): Option[Long] = {
    if (end >= nums.length) None
    else {
      val cs = nums.slice(start, end).sum
      if (cs < badv) findContiguousSum(start, end+1, badv, nums)
      else if (cs > badv) findContiguousSum(start+1, end, badv, nums)
      else Some(nums.slice(start, end).min + nums.slice(start, end).max)
    }
  }

}
