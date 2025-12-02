package day2

import day2.IdPair.{isInvalid, isInvalidPart2}

import scala.io.Source

case class IdPair(start: Long, finish: Long):
  def invalids: Seq[Long] =
    (start to finish).filter(isInvalid)
  def invalidsPart2: Seq[Long] =
    (start to finish).filter(isInvalidPart2)

object IdPair:
  def isInvalid(s: Long): Boolean =
    val asStr = s.toString
    val len = asStr.length
    len % 2 == 0 && asStr.take(len / 2) == asStr.drop(len / 2)
  def isInvalidPart2(s: Long): Boolean =
    val asStr = s.toString
    val len = asStr.length
    (1 to len/2).map { l =>
      if (len % l == 0) {
        val splits = asStr.grouped(l).toSet
        splits.size == 1
      } else false
    }.count(_ == true) > 0
end IdPair

def parseInput(input: Seq[String]): Seq[IdPair] =
  input.flatMap(line =>
    line.split(",").map {
    pair =>
      val spl = pair.split("-")
      IdPair(spl.head.toLong, spl.tail.head.toLong)
    })

@main
def main(): Unit =
  val input = readFile("src/main/scala/day2/input")
  val idPairs = parseInput(input)
  println(idPairs.flatMap(_.invalids).sum)
  println(idPairs.flatMap(_.invalidsPart2).sum)

def readFile(filename: String): Seq[String] =
  val bufferedSource = Source.fromFile(filename)
  val result         = bufferedSource.getLines.toSeq
  bufferedSource.close
  result
