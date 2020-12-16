import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Main {

  def main(args: Array[String]): Unit = {
    val nums = List(15,5,1,4,7,0).map(_.toLong)
    println(getNthNum(nums, 30000000))
  }
  def getNthNum(nums: List[Long], end: Int): Long = {
    val spoken_map = nums.zipWithIndex.map(v => (v._1, Queue(v._2 + 1))).toMap
    val cur_turn = nums.length + 1
    val last_spoken = nums.last
    @tailrec
    def run(cm: Map[Long, Queue[Int]], turn: Int, ls: Long): Long = {
















      if (turn % 1000000 == 0) println(turn)
      if (turn == end + 1) ls
      else if (cm(ls).length == 1) run(cm.updated(0.toLong, cm.getOrElse(0.toLong, Queue()).appended(turn)), turn+1, 0.toLong)
      else {
        val (ppv, q) = cm(ls).dequeue
        val cmu = cm.updated(ls, q)
        val next_v = (turn - 1) - ppv
        run(cmu.updated(next_v, cmu.getOrElse(next_v, Queue()) :+ (turn)), turn+1, next_v)
      }
    }
    run(spoken_map, cur_turn, last_spoken)
  }

}
