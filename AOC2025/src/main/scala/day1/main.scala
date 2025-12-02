package day1

import scala.io.Source

enum Direction:
  case Left, Right

case class Dial(state: Int, passedZero: Int):
  def rotateLeft =
    if (state == 0) Dial(99, passedZero)
    else Dial(state - 1, if (state == 1) passedZero + 1 else passedZero)
  def rotateRight =
    if (state == 99) Dial(0, passedZero + 1)
    else Dial(state + 1, passedZero)
  def apply(r: Rotation) =
    (1 to r.degrees).foldLeft[Dial](this) { case (acc, elt) =>
      if (r.d == Direction.Right) acc.rotateRight
      else acc.rotateLeft
    }

def parseInput(input: Seq[String]) =
  input.map(line =>
    val degrees = line.drop(1).toInt
    line.head match {
      case 'L' => Rotation(Direction.Left, degrees)
      case 'R' => Rotation(Direction.Right, degrees)
    })

@main
def main(): Unit =
  val input = readFile("src/main/scala/day1/input")
  val rotations: Seq[Rotation] = parseInput(input)
  val dial = Dial(50, 0)
  val dials = rotations.foldLeft[Seq[Dial]](Seq(dial)) { case (acc, elt) =>
    acc :+ acc.reverse.head.apply(elt)
  }
  println(dials.count(_.state == 0))
  println(dials.map(_.passedZero).reverse.head)

case class Rotation(d: Direction, degrees: Int)

def readFile(filename: String): Seq[String] =
  val bufferedSource = Source.fromFile(filename)
  val result         = bufferedSource.getLines.toSeq
  bufferedSource.close
  result
