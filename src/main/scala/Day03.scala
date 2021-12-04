import Utils._

object Day03 extends App {
  val input = readFrom("test.txt")
  val input2 = readFrom("test.txt")
  val input3 = readFrom("test.txt")

  println(binaryDiagnosticGama(input) * binaryDiagnosticEpsilon(input2))

  def binaryDiagnosticGama(input: Iterator[String]) = {
    parserBinaryToDec(
      findMostCommonBinary(
        parserIteratorToBinary(input)
      )
    ).sum
  }

  def binaryDiagnosticEpsilon(input: Iterator[String]) = {
    parserBinaryToDec(
      findLeastCommonBinary(
        parserIteratorToBinary(input)
      )
    ).sum
  }

  def parserIteratorToBinary(input: Iterator[String]): List[List[Int]] = {
    input.map(a => a.toCharArray.toList).toList
      .transpose
      .map(_.map(a => Integer.parseInt(a.toString))
        .map {
          case 0 => -1
          case 1 => 1
        }
      )
  }

  def parserBinaryToDec(input: List[Int]): List[Double] = {
    input.reverse
      .zipWithIndex
      .map {
        case (a, b) => a * (math.pow(2, b))
      }
  }

  def findMostCommonBinary(input: List[List[Int]]): List[Int] = {
    input.map(a => a.sum)
      .map {
        case a if a >= 0 => 1
        case a if a < 0 => 0
      }
  }

  def findLeastCommonBinary(input: List[List[Int]]): List[Int] = {
    input.map(a => a.sum)
      .map {
        case a if a > 0 => 0
        case a if a <= 0 => 1
      }
  }

}
