object Main {

  def main(args: Array[String]): Unit = {
    val nums = List(15,5,1,4,7,0).map(_.toLong)
    println(getNthNum(nums, 2020))
  }
  def getNthNum(nums: List[Long], end: Int): Long = {
    val spoken_map = nums.zipWithIndex.map(v => (v._1, List(v._2 + 1))).toMap
    val cur_turn = nums.length + 1
    val last_spoken = nums.last
    def run(cm: Map[Long, List[Int]], turn: Int, ls: Long): Long = {
      if (turn == end + 1) ls
      else if (cm(ls).length == 1) run(cm.updated(0.toLong, cm.getOrElse(0.toLong, List()).appended(turn)), turn+1, 0.toLong)
      else {
        val next_v = (turn - 1) - cm(ls).takeRight(2).head
        run(cm.updated(next_v, cm.getOrElse(next_v, List()).appended(turn)), turn+1, next_v)
      }
    }
    run(spoken_map, cur_turn, last_spoken)
  }

}
