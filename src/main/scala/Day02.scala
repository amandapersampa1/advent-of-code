import Utils.readFrom

object Day02 extends App {

  val input = readFrom("Day02.txt")
  val r = calculateMovement(input)
  println(r)
  println(r._1*r._2)

  private def calculateMovement(input: Iterator[String]): (Int, Int) = {
    val inp = input.map( i => {
        i.split(" ") match {
          case Array(k, v) => (k, v.toInt)
        }
      }).toList
    checkMovement(0, inp, 0, 0)
  }

  private def checkMovement(i: Int, input: List[(String, Int)], h: Int, v: Int): (Int, Int) = {
    if (i < input.size && "forward".equals(input(i)._1))
      checkMovement(i + 1, input, (h + input(i)._2), v)
    else if (i < input.size && "down".equals(input(i)._1))
      checkMovement(i + 1, input, h, v + input(i)._2)
    else if (i < input.size && "up".equals(input(i)._1))
      checkMovement(i + 1, input, h, v - input(i)._2)
    else {
      (h, v)
    }
  }

}
