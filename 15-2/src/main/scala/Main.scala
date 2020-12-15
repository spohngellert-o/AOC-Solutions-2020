import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    val nums = List(15,5,1,4,7,0).map(_.toLong)
    println(getNthNum(nums, 30000000))
  }
  def getNthNum(nums: List[Long], end: Int): Long = {
    val spoken_map = nums.zipWithIndex.map(v => (v._1, (v._2 + 1, v._2 + 1))).toMap
    val cur_turn = nums.length + 1
    val last_spoken = nums.last
    @tailrec
    def run(cm: Map[Long, (Int, Int)], turn: Int, ls: Long): Long = {
      if (turn % 1000000 == 0) println(turn)
      if (turn == end + 1) ls
      else {
        val next_v = (turn - 1) - cm(ls)._1
        run(cm.updated(next_v, (cm.getOrElse(next_v, (turn, turn))._2, turn)), turn+1, next_v)
      }
    }
    run(spoken_map, cur_turn, last_spoken)
  }

}
