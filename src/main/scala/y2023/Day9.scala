package y2023

import scala.annotation.tailrec

object Day9:
  def main(args: Array[String]): Unit =
    val data = io.Source
      .fromResource("2023/9.txt")
      .getLines()
      .map(_.split(' ').map(_.toInt).toVector)
      .toVector

    println(part1(data))
    println(part2(data))

  def part1(line: Seq[Seq[Int]]): Int =
    line.map(nextValue).sum

  def part2(line: Seq[Seq[Int]]): Int =
    line
      .map(_.reverse)
      .map(nextValue)
      .sum

  private def nextValue(xs: Seq[Int]) = xs.last + calcDiff(xs)

  @tailrec
  private def calcDiff(values: Seq[Int], lastAcc: Int = 0): Int =
    val diffs = values
      .sliding(2)
      .map { case Seq(a, b) => b - a }
      .toSeq

    val acc = lastAcc + diffs.last
    if diffs.forall(_ == 0) then acc
    else calcDiff(diffs, acc)
