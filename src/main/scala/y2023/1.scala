package y2023

@main
def main(): Unit =
  val data = io.Source
    .fromResource("2023/1.txt")
    .mkString

  println(part1(data))
  println(part2(data))

def part1(data: String) =
  data.linesIterator
    .map(_.filter(_.isDigit).map(_.asDigit))
    .map(xs => xs.head * 10 + xs.last)
    .sum

def part2(data: String) =
  val digitRegex = (digitMap.keys ++ Seq("\\d")).mkString("|").r

  data.linesIterator
    .map: line =>
      val matches = (for
        s <- line.tails
        m <- digitRegex.findPrefixOf(s)
      yield m).toSeq

      val first = stringToDigit(matches.head)
      val last = stringToDigit(matches.last)
      first * 10 + last
    .sum

val digitMap = Map(
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9
)
val stringToDigit = digitMap ++ (1 to 9).map(i => i.toString -> i)
