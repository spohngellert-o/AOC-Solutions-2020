import scala.io.Source

object Main {
  case class Ship(xShip: Int, yShip: Int, xWay: Int, yWay: Int) {
    def act(action: Char, mag: Int): Ship = {
      action match {
        case 'N' => Ship(xShip, yShip, xWay, yWay + mag)
        case 'S' => Ship(xShip, yShip, xWay, yWay - mag)
        case 'E' => Ship(xShip, yShip, xWay + mag, yWay)
        case 'W' => Ship(xShip, yShip, xWay - mag, yWay)
        case 'R' => Ship(xShip, yShip, rot(mag, true), rot(mag, false))
        case 'L' => Ship(xShip, yShip, rot(360-mag, true), rot(360-mag, false))
        case 'F' => Ship(xShip + (mag * xWay), yShip + (mag * yWay), xWay, yWay)
      }
    }
    private def rot(mag: Int, x: Boolean): Int = {
      if (x) List(yWay, -xWay, -yWay, xWay)((mag/90) - 1) else List(-xWay, -yWay, xWay, yWay)((mag/90) - 1)
    }

    def getManDist: Int = {
      Math.abs(xShip) + Math.abs(yShip)
    }

    override def toString: String = {
      (xShip, yShip, xWay, yWay).toString()
    }
  }
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    println(file.getLines.foldLeft(Ship(0, 0, 10, 1))((ship, v) => ship.act(v.head, v.tail.toInt)).getManDist)
    file.close()
  }
}
