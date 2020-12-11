import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val seats = file.getLines().toList.map(_.toCharArray.appended('.').prepended('.'))
    val seats_bordered = seats.appended((".".toString * seats.head.length).toCharArray).prepended((".".toString * seats.head.length).toCharArray)
    println(getStableSeating(seats_bordered))
    file.close
  }

  @tailrec
  def getStableSeating(seats: List[Array[Char]]): Int = {
    def getNextSeating(x: Int, y: Int, cur: Char): Char = {
      val nearby = seats.slice(x - 1, x + 2).flatMap(_.slice(y - 1, y + 2))
      cur match {
        case '#' => if(nearby.count(_ == '#') >= 5) 'L' else '#'
        case 'L' => if(nearby.count(_ == '#') == 0) '#' else 'L'
        case '.' => '.'
      }
    }
    val next_seating = seats.zipWithIndex.map({case (element, x) =>
      element.zipWithIndex.map({case (cur, y) => getNextSeating(x, y, cur)})
    })
    if (next_seating.zip(seats).forall({case (n, p) => n sameElements p})) next_seating.map(_.count(_ == '#')).sum
    else getStableSeating(next_seating)
  }

}
