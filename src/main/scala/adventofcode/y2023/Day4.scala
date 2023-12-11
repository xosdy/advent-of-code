package adventofcode.y2023

object Day4:
  def main(args: Array[String]): Unit =
    val cards = parse(io.Source.fromResource("2023/4.txt").getLines)
    println(part1(cards))
    println(part2(cards))

  private case class Card(id: Int, winning: Set[Int], mine: Set[Int]):
    def winningSize: Int = winning.intersect(mine).size

  private def parse(lines: Iterator[String]): Vector[Card] =
    def parseNumbers(s: String) = s.trim.split(" +").map(_.toInt).toSet
    lines
      .map:
        case s"Card $i: $winnings|$mine" =>
          Card(i.trim.toInt, parseNumbers(winnings), parseNumbers(mine))
      .toVector

  private def part1(cards: Seq[Card]): Int =
    cards
      .map(_.winningSize)
      .map(size => if size > 0 then math.pow(2, size - 1).toInt else 0)
      .sum

  private def part2(cards: Seq[Card]): Int =
    cards
      .map(_.winningSize)
      .foldLeft((0, Seq(1))):
        case ((totalCards, copies), winning) =>
          val currCopy = copies.head
          val restCopy = copies
            .drop(1)
            .padTo(math.max(1, winning), 1)
            .zipWithIndex
            .map((copy, i) => if i < winning then copy + currCopy else copy)
          (totalCards + currCopy, restCopy)
      ._1
