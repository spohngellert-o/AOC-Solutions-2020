import scala.io.Source

object Main {
  case class Ship(direction: Int, x: Int, y: Int) {
    def act(action: Char, mag: Int): Ship = {
      action match {
        case 'N' => Ship(direction, x, y+mag)
        case 'S' => Ship(direction, x, y-mag)
        case 'E' => Ship(direction, x+mag, y)
        case 'W' => Ship(direction, x-mag, y)
        case 'R' => Ship((direction + (mag / 90)) % 4, x, y)
        case 'L' => Ship((direction + ((360 - mag) / 90)) % 4, x, y)
        case 'F' => act(List('E', 'S', 'W', 'N')(direction), mag)
      }
    }

    def getManDist: Int = {
      Math.abs(x) + Math.abs(y)
    }
  }
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    println(file.getLines.foldLeft(Ship(0, 0, 0))((ship, v) => ship.act(v.head, v.tail.toInt)).getManDist)
    file.close()
  }
}
