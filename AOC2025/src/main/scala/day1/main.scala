package day1

import scala.io.Source

enum Direction:
  case Left, Right

def parseInput(input: Seq[String]) =
  input.map(line =>
    val degrees = line.drop(1).toInt
    line.head match {
      case 'L' => Rotation(Direction.Left, degrees)
      case 'R' => Rotation(Direction.Right, degrees)
    })

@main
def main(): Unit =
  val input = readFile("src/main/scala/day1/example")
  val rotations: Seq[Rotation] = parseInput(input)
  val start = 50
  rotations.foreach(println)
  val vals = rotations.foldLeft[Seq[Int]](Seq(start)) { case (acc,elt) =>
    acc :+ elt.apply(acc.reverse.head)
  }
  println(vals.count(_ == 0))
  val vals2 = rotations.foldLeft[Seq[(Int, Int)]](Seq((start, 0))) { case (acc,elt) =>
    acc :+ elt.apply2(acc.reverse.head._1)
  }
  println(vals2.count(_._1 ==0))
    println(vals2.map(_._2).sum)


case class Rotation(d: Direction, degrees: Int):
  def apply(start: Int) =
    val intermediate = d match {
      case Direction.Left => start - degrees
      case Direction.Right => start + degrees
    }
    (100 + intermediate) % 100
  def apply2(start: Int): (Int,Int) =
    d match {
      case Direction.Left =>
        val dist = start - degrees
        ((start - degrees) % 100, (start - degrees) / 100)
      case Direction.Right =>
        ((100 + start + degrees) % 100, (start + degrees) / 100)
    }

def readFile(filename: String): Seq[String] =
  val bufferedSource = Source.fromFile(filename)
  val result         = bufferedSource.getLines.toSeq
  bufferedSource.close
  result
