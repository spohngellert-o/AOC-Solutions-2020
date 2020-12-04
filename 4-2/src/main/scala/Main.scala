import scala.collection.immutable.HashMap
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val data = file.mkString.split("\n\n")
    file.close()

    def isDigLims(v: String, l: Int, u: Int): Boolean = {
      v.forall(_.isDigit) && v.toInt >= l && v.toInt <= u
    }

    val fields = HashMap(
      "byr" -> ((v: String) => v.length == 4 && isDigLims(v, 1920, 2002)),
      "iyr" -> ((v: String) => v.length == 4 && isDigLims(v, 2010, 2020)),
      "eyr" -> ((v: String) => v.length == 4 && isDigLims(v, 2020, 2030)),
      "hgt" -> ((v: String) => {
        if (v.endsWith("in")) isDigLims(v.split("in").head, 59, 76)
        else if (v.endsWith("cm")) isDigLims(v.split("cm").head, 150, 193)
        else false
      }),
      "ecl" -> ((v: String) => Array("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains v),
      "pid" -> ((v: String) => v.length == 9 && v.forall(_.isDigit)),
      "hcl" -> ((v: String) => v.startsWith("#") && v.split("#").last.length == 6 &&
        v.split("#").last.forall(c => c.isDigit || List('a', 'b', 'c', 'd', 'e', 'f').contains(c))))
    println(data.count(ppt =>
      fields.forall(
        { case (field, validator) =>
          ppt.contains(field) && validator(ppt.split(field + ":")(1).split("^ |\n").head)})))

  }
}
