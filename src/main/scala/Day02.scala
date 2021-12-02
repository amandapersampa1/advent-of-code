import Utils.readFrom

object Day02 extends App {

  val input = readFrom("Day02.txt")
  val r = calculateMovement2(input)
  println(r)
  println(r._1 * r._2)

  private def calculateMovement(input: Iterator[String]): (Int, Int) = {
    val inp = input.map(i => {
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

  private def calculateMovement2(input: Iterator[String]): (Int, Int, Int) = {
    val inp = input.map(i => {
      i.split(" ") match {
        case Array(k, v) => (k, v.toInt)
      }
    }).toList
    checkMovement2(0, inp, 0, 0, 0)
  }

  private def checkMovement2(i: Int, input: List[(String, Int)], h: Int, v: Int, aim: Int): (Int, Int, Int) = {
    if (i < input.size) {
      var newH = h
      var newV = v
      var newAim = aim

      if ("forward".equals(input(i)._1)) {
        if (aim == 0) {
          newH += input(i)._2
        } else {
          newH +=  input(i)._2
          newV += input(i)._2 * aim
        }
      } else if ("down".equals(input(i)._1)){
        newAim +=  input(i)._2
      }
      else if ("up".equals(input(i)._1)) {
        newAim -=  input(i)._2
      }
      checkMovement2(i + 1, input, newH, newV, newAim)
    } else {
      (h, v, aim)
    }
  }

}
