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
    val valid_tix = nt.split("\n").tail.filter(line => {
      line.split(",").forall(v => isValid(v.toInt, rule_map))
    }).map(_.split(",").map(_.toInt))
    val inds = getFieldInds(valid_tix, rule_map)
    val ticket = yt.split("\n").last.split(",").map(_.toLong)
    println(inds.filter({case (k, _) => k.startsWith("departure")}).values.map(ticket(_)).product)
    file.close
  }

  def isValid(v: Int, rules: Map[String, Array[(Int, Int)]]): Boolean = {
    rules.exists({ case (k, ranges) => {
      ranges.exists({ case (mn, mx) => v >= mn && v <= mx })
    }
    })
  }

  def getFieldInds(tix: Array[Array[Int]], rules: Map[String, Array[(Int, Int)]]): Map[String, Int] = {
    var inds = tix.head.zipWithIndex.map({case (v, ind) => ind -> rules.keys.toList}).toMap
    tix.foreach(vals => {
      vals.zipWithIndex.foreach({ case (v, ind) => {
        rules.foreach({ case (k, ranges) =>
          {
            if (!ranges.exists({ case (mn, mx) => v >= mn && v <= mx })) {
              inds = inds.updated(ind, inds(ind).filter(curk => curk != k))
            }
          } })
      }
      })
    })
    inds.toList.sortBy({case (ind, valks) => valks.size}).foldLeft(Map[String, Int](), Set[String]())({case ((m, used), (ind, valks)) => {
      val cur_k = valks.filter(k => ! used.contains(k)).head
      (m + (cur_k -> ind), used + cur_k)
    }})._1
  }
}
