import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
      println("Hello, world!")
      val filename = "input"
      var nums = Array[Int]()
      for (line <- Source.fromFile(filename).getLines) {
        nums = nums :+ line.toInt
      }
      val v1 = nums.foldLeft(Array[Int](), 0)((acc, v) => {
        if (acc._1.contains(v)) {
          (acc._1, v)
        }
        else {
          (acc._1 :+ 2020-v, acc._2)
        }
      })._2
      System.out.println(v1)
      System.out.println(v1 * (2020 - v1))
    }

  }
