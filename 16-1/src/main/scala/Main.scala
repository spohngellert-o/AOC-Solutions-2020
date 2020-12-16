import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val (rules, yt, nt) = file.mkString.split("\n\n") match {
      case Array(a, b, c) => (a, b, c)
    }
    val rule_map = rules.split("\n").map((line) => {
      val rn = line.split(": ").head
      val cur_rule = line.split(": ").last
      val r1 = cur_rule.split(" or ").head
      val r2 = cur_rule.split(" or ").last
      (rn, cur_rule.split(" or ").map(rl => (rl.split("-").head.toInt, rl.split("-").last.toInt)))
    }).toMap
    println(nt.split("\n").tail.map(line => {
      line.split(",").filter(v => !isValid(v.toInt, rule_map)).map(_.toInt).sum
    }).sum)
    file.close
  }

  def isValid(v: Int, rules: Map[String, Array[(Int, Int)]]): Boolean = {
    rules.exists({case (k, ranges) => {
      ranges.exists({case (mn, mx) => v >= mn && v <= mx})
    }})
  }

}
