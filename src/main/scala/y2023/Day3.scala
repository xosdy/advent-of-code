package y2023

object Day3:
  def main(args: Array[String]): Unit =
    val grid = parse(io.Source.fromResource("2023/3.txt").getLines())
    println(part1(grid))
    println(part2(grid))

  enum Object:
    case Number(value: Int)
    case Symbol(value: Char)

  private case class Vec2(x: Int, y: Int)
  private type Grid = Map[Vec2, Object]
  private def parse(lines: Iterator[String]): Grid =
    val objectRegex = raw"(\d+)|[^.\d]".r
    (for
      (line, y) <- lines.zipWithIndex
      m         <- objectRegex.findAllMatchIn(line)
    yield Vec2(m.start, y) -> (m.matched.toIntOption match
      case Some(number) => Object.Number(number)
      case _            => Object.Symbol(m.matched(0))
    )).toMap

  private def adjacentPoints(point: Vec2, length: Int): Seq[Vec2] =
    for
      x <- point.x - 1 to point.x + length
      y <- point.y - 1 to point.y + 1
      if !(x == point.x && y == point.y)
    yield Vec2(x, y)

  private def hasAdjacentSymbol(grid: Grid, point: Vec2, length: Int): Boolean =
    adjacentPoints(point, length).exists: adjacentPoint =>
      grid.get(adjacentPoint) match
        case Some(Object.Symbol(_)) => true
        case _                      => false

  private def part1(grid: Grid): Int =
    grid.collect {
      case (point, Object.Number(n))
          if hasAdjacentSymbol(grid, point, n.toString.length) =>
        n
    }.sum

  private def adjacentNumbers(grid: Grid, point: Vec2): Seq[Int] =
    grid.collect {
      case (numberPoint, Object.Number(n))
          if point.x >= numberPoint.x - 1
            && point.x <= numberPoint.x + n.toString.length
            && point.y >= numberPoint.y - 1
            && point.y <= numberPoint.y + 1 =>
        n
    }.toSeq

  private def part2(grid: Grid): Int =
    grid
      .collect:
        case (point, Object.Symbol('*')) =>
          val numbers = adjacentNumbers(grid, point)
          if numbers.length == 2 then numbers.product
          else 0
      .sum
