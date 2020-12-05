import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val ids = file.getLines().toList

    val id_nums = ids.map(id => getIdFromStr(id))
    var possible_seats = (0 to (8 * 127) + 7).map(v => false)
    id_nums.foreach(id => possible_seats = possible_seats.updated(id, true))
    println(possible_seats.count(v => !v))
    println(possible_seats.indexOfSlice(List(true, false, true)) + 1)
    file.close()

  }
  def getIdFromStr(id_str: String): Int = {
    val row = id_str.slice(0, 7).foldLeft((64, 0))({case ((mul, tot), v) => (mul/2, tot + (mul * (if (v == 'B') 1 else 0)))})._2
    val col = id_str.slice(7, 11).foldLeft((4, 0))({case ((mul, tot), v) => (mul/2, tot + (mul * (if (v == 'R') 1 else 0)))})._2
    return 8 * row + col
  }

}
