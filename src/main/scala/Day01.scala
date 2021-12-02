import Utils._

object Day01 extends App {

  val input = readFrom("Day01.txt")
  println(calculateIncrease(input))

  private def calculateIncrease(input: Iterator[String]): Int = {
    checkIncrease(0, input.toList.map(_.toInt), 0)
  }

  private def checkIncrease(i: Int, input: List[Int], sum: Int): Int = {
    if (i < input.length-3) {
      if (input(i)+input(i+1)+input(i+2) < input(i+1)+input(i+2)+input(i+3))
        checkIncrease(i+1, input, sum + 1)
      else
        checkIncrease(i+1, input, sum)
    }
    else sum
  }

}