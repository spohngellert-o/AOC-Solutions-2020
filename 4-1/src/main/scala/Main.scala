import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input")
    val data = file.mkString.split("\n\n")
    file.close()
    val fields = Array("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    println(data.size)
    println(data.filter(ppt => fields.forall(pfx => ppt.contains(pfx + ":"))).size)
  }
}
