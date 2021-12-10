object day5 {
  def main(args: Array[String]): Unit = {
    getLines
      .foreach(println)
  }

  case class line(start: (Int, Int), end: (Int, Int))

  def getLines: Iterator[line] = {
    """(\d)+""".r.findAllMatchIn(readInput())
      .map(_.matched.toInt)
      .sliding(4, 4)
      .map(m => line((m(0), m(1)), (m(2), m(3))))
  }

  def readInput(): String = scala.io.Source.fromResource("day5.txt").getLines().mkString(" ")
}
