package adventofcode.y2023

object Day2:
  def main(args: Array[String]): Unit =
    val data = io.Source
      .fromResource("2023/2.txt")
      .getLines()
      .toVector

    val games = parse(data)
    println(part1(games))
    println(part2(games))

  def parse(data: Vector[String]): Vector[Game] =
    data.map(parseGame)

  def part1(games: Vector[Game]): Int =
    games.filter(_.isPossible).map(_.id).sum

  def part2(games: Vector[Game]): Int =
    games.map(_.powerOfMinCubes).sum

  case class Game(id: Int, rounds: Vector[Round]):
    def isPossible: Boolean = rounds.forall(round =>
      round.red <= 12 && round.green <= 13 && round.blue <= 14
    )

    def powerOfMinCubes: Int =
      rounds.map(_.red).max * rounds.map(_.green).max * rounds.map(_.blue).max

  case class Round(red: Int, green: Int, blue: Int)

  def parseGame(game: String): Game = game match
    case s"Game $id: $rounds" =>
      Game(id.toInt, rounds.split("; ").map(parseRound).toVector)

  def parseRound(round: String): Round =
    round
      .split(", ")
      .foldLeft(Round(0, 0, 0)): (acc, cube) =>
        cube match
          case s"$x red"   => acc.copy(red = x.toInt)
          case s"$x green" => acc.copy(green = x.toInt)
          case s"$x blue"  => acc.copy(blue = x.toInt)
