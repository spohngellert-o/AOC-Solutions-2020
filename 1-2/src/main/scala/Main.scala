import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    val filename = "input"
    var nums = Array[Int]()
    for (line <- Source.fromFile(filename).getLines) {
      nums = nums :+ line.toInt
    }
    val pairs = nums.foldLeft((Array[Tuple2[Int, Int]](), Array[Int]()))((acc, v) => {
      (Array.concat(acc._1, acc._2.map((cur) => (cur, v))), acc._2 :+ v)
    })._1

    val triplet = pairs.foldLeft((0, 0, 0))((acc, v) => {
      val pairSum = v._1 + v._2
      if (nums.contains(2020 - pairSum)) {
        (v._1, v._2, 2020 - pairSum)
      }
      else {
        acc
      }
    })
    System.out.println(triplet._1 * triplet._2 * triplet._3)
  }

}