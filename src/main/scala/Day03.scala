import Utils._

object Day03 extends App {
  val input = readFrom("test.txt")
  val input2 = readFrom("test.txt")
  val input3 = readFrom("test.txt")
  val input4 = readFrom("test.txt")

  println(binaryDiagnosticGama(input))
  println(binaryDiagnosticEpsilon(input2))
  println(binaryOxygenGeneratorRating(input3))
  println(binaryCO2ScrubberRating(input4))

  def binaryOxygenGeneratorRating(input: Iterator[String]): Double = {
    val binary = List()
    val newInput = findThreeMostCommonBinary(parserIteratorToBinary(input), binary).map {
      case -1 => 0
      case 1 => 1
    }

    parserBinaryToDec(newInput.reverse).sum
  }

  def binaryCO2ScrubberRating(input: Iterator[String]): Double = {
    val binary = List()
    val newInput = findThreeLeastCommonBinary(parserIteratorToBinary(input), binary).map {
      case -1 => 0
      case 1 => 1
    }

    parserBinaryToDec(newInput.reverse).sum
  }

  def findThreeLeastCommonBinary(input: List[List[Int]], binary: List[Int]): List[Int] = {
    if (input.nonEmpty) {
      val newBinary = (if (input.head.sum <= 0) 1 else -1) :: binary
      val newInput = input.transpose.filter(a => a.head == newBinary.head).transpose
      if (newInput.nonEmpty)
        findThreeMostCommonBinary(newInput.tail, newBinary)
      else newBinary
    }
    else binary
  }

  def findThreeMostCommonBinary(input: List[List[Int]], binary: List[Int]): List[Int] = {
    if (input.nonEmpty) {
      val newBinary = (if (input.head.sum >= 0) 1 else -1) :: binary
      val newInput = input.transpose.filter(a => a.head == newBinary.head).transpose
      if (newInput.nonEmpty)
        findThreeMostCommonBinary(newInput.tail, newBinary)
      else newBinary
    }
    else binary
  }

  def binaryDiagnosticGama(input: Iterator[String]) = {
    parserBinaryToDec(
      findMostCommonBinary(
        parserIteratorToBinary(input).map(a => a.sum)
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

  def findMostCommonBinary(input: List[Int]): List[Int] = {
    input.map {
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
