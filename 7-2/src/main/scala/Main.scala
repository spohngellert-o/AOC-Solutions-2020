import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val file = Source.fromFile("input")
    val rules = file.getLines.toList
    val ruleMap = rules.map(getRule).toMap
    println(countBags(ruleMap.getOrElse("shiny gold", Array.empty), ruleMap))
    file.close()
  }
  def getRule(rulestr: String): (String, Array[(Int, String)]) = {
    val container = rulestr.split(" bags").head
    if (!rulestr.contains("no other bags")) {
      val contained = rulestr.split("contain ").last.split(",").map(
        bt => {
          (bt.trim.split(" ").head.toInt, bt.trim.split(" ").slice(1, 3).mkString(" ").replace(".", "").trim)
        }
      )
      (container, contained)
    }
    else (container, Array[(Int, String)]())
  }
  def canContainGold(containerMap: Map[String, Array[(Int, String)]], container: String): Boolean = {
    val contains = containerMap.getOrElse(container, Array.empty)
    if (contains.map(_._2).contains("shiny gold")) true
    else contains.exists({
      case (_, contained) => canContainGold(containerMap, contained)
      case _ => false
    })
  }
  def countBags(bags: Array[(Int, String)], rules: Map[String, Array[(Int, String)]]): Int = {
    bags.map({
      case (nbags, bag) => nbags + nbags * countBags(rules.getOrElse(bag, Array.empty), rules)
      case _ => 0
    }).sum
  }
}
