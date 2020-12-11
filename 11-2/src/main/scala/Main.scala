import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val seats = file.getLines().toList.map(_.toCharArray.appended('.').prepended('.'))
    val seats_bordered = seats.appended((".".toString * seats.head.length).toCharArray).prepended((".".toString * seats.head.length).toCharArray)
    println(getStableSeating2(seats_bordered))
    file.close
  }

  @tailrec
  def getStableSeating2(seats: List[Array[Char]]): Int = {
    def getNextSeating(x: Int, y: Int, cur: Char): Char = {
      cur match {
        case '.' => '.'
        case _ => {
          val directions = List((-1, 0), (0, 1), (1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1))
          val nrows = seats.length
          val ncols = seats.head.length
          val nearby = directions.map({case (dx, dy) => {
            var curx = x + dx
            var cury = y + dy
            var curc = '.'
            while (curc == '.' && 0 <= curx && curx < nrows && 0 <= cury && cury < ncols) {
              curc = seats(curx)(cury)
              curx = curx + dx
              cury = cury + dy
            }
            curc
          }})
          cur match {
            case '#' => if(nearby.count(_ == '#') >= 5) 'L' else '#'
            case 'L' => if(nearby.count(_ == '#') == 0) '#' else 'L'
          }
        }
      }
    }
    val next_seating = seats.zipWithIndex.map({case (element, x) =>
      element.zipWithIndex.map({case (cur, y) => getNextSeating(x, y, cur)})
    })
    if (next_seating.zip(seats).forall({case (n, p) => n sameElements p})) next_seating.map(_.count(_ == '#')).sum
    else getStableSeating2(next_seating)
  }

}
